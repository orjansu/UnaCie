{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module AbstractMachine
  ( evalEval       -- Evaluate a term and return the result.
  , evalEvalSugar  -- Evaluate a term and return the result sugared.
  , runEval        -- Evaluate a term and return the result + evaluation cost.
  , runEvalSugar   -- Evaluate a term and return the sugared result + evaluation cost.
  ) where

import Classes       ( AddBinders(..), SafeNames(..)
                     , TimeCost(..)
                     )
import Crumb         (Crumb)
import CtxAST        ( Alt(..), Bind(..), Ctx(..)
                     , Name, Term
                     )
import CtxUtils      (isDatatype, isVal)
import KureContext   (emptyKureContext)
import KureExtra     (concatMapT)
import Normalisation (sugarR)
import Runners       (evalTMEnv, runTMEnv)
import Subst         ( safeBindersR, sieveT, substCaseR
                     , substTryR
                     )
import TransUtils    (deleteUnusedLetBindingsR)
import Universes     (U)
import Utils         (names)

import Control.Arrow            ((>>>))
import Control.Monad.State.Lazy (MonadState, modify)
import Data.List                (partition)
import Data.Maybe               (catMaybes)
import qualified Data.Map as Map

import Language.KURE.ExtendableContext (ExtendContext)
import Language.KURE
  ( ExtendPath
  , LocalPath
  , MonadCatch
  , ReadPath
  , Rewrite
  , Transform
  , Walker
  , applyR
  , applyT
  , exposeT
  , guardMsg
  , idR
  , prefixFailMsg
  , repeatR
  , rewrite
  , transform
  )

{-
  Information:
  -----------------------------------------------------------------------------
  - An implementation of Sestoft's mark 1 abstract machine for evaluating
    terms.
-}

-- Evaluation environment extends the KureEnv in order to count evaluation
-- steps, i.e., number of transitions of the abstract machine.
data EvalEnv = EvalEnv { steps :: Int, sn :: [Name] }

instance Show EvalEnv where
  show = show . steps

-- EvalEnv has methods for generating safe names. These are needed for
-- alpha-renaming during beta-reduction.
instance SafeNames EvalEnv where
  getSafes          = sn
  modifySafes f env = env { sn = f (sn env) }
  fresh nss env     = case dropWhile (`elem` nss) (sn env) of
    []         -> Nothing
    (xs : xss) -> Just (xs, env { sn = xss } )

-- EvalEnv has methods for recording time cost during evaluation.
instance TimeCost EvalEnv where
  incCost  env = env { steps = succ (steps env) }
  decCost  env = env { steps = pred (steps env) }
  nullCost env = env { steps = 0 }

-- Stack tokens: --------------------------------------------------------------

-- Stack tokens are used to guide the abstract machine when the term being
-- evaluated reaches value form. See paper for more details.
data Token = Update Name          -- Update marker
           | Variable Name        -- Stack variables
           | Alts [Alt]           -- Case alternatives
             deriving (Eq, Show)

type Stack = [Token]
type Heap  = Map.Map Name Term    -- Essentially a set of bindings
type Conf  = (Heap, Term, Stack)  -- Abstract machine configuration

-- Top-level functions: -------------------------------------------------------

-- Evaluate a term and return the result and number of steps taken.
runEval :: Term -> (Either String Term, Int)
runEval t = fmap steps $ runTMEnv (applyT evalNoSugarT emptyKureContext
             (initConf t)) initEvalEnv

-- Evaluate a term and return the sugared result and number of steps taken.
runEvalSugar :: Term -> (Either String Term, Int)
runEvalSugar t = fmap steps $ runTMEnv (applyT evalSugarT emptyKureContext
                   (initConf t)) initEvalEnv


-- As above but only return the result.
evalEval :: Term -> Either String Term
evalEval t = evalTMEnv (applyT evalNoSugarT emptyKureContext
              (initConf t)) initEvalEnv

-- Evaluate a term and sugar the result.
evalEvalSugar :: Term -> Either String Term
evalEvalSugar t = evalTMEnv (applyT evalSugarT emptyKureContext
                   (initConf t)) initEvalEnv

-- Transformations corresponding to above functions: --------------------------

-- Evaluate configuration, garbage collect and sugar resulting term.
evalSugarT :: ( SafeNames s, ExtendPath c Crumb, ReadPath c Crumb
              , AddBinders c, MonadState s m, TimeCost s, MonadCatch m
              , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
              , AddBinders (ExtendContext c (LocalPath Crumb)) )
              => Transform c m Conf Term
evalSugarT  = prefixFailMsg "evalSugarT failed: " $
              repeatR (evalR [])
                >>> checkConfR    -- Ensures the final config. is valid because
                                  -- repeatR returns config. before failure.
                >>> evalHeapR     -- Force heap evaluation, needed for sugar
                >>> confToTermT
                >>> sugarR
                >>> deleteUnusedLetBindingsR

-- Evaluate configuration and garbage collect only.
evalNoSugarT :: ( SafeNames s, ExtendPath c Crumb, ReadPath c Crumb
                , AddBinders c, MonadState s m, TimeCost s, MonadCatch m
                , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                , AddBinders (ExtendContext c (LocalPath Crumb)) )
                => Transform c m Conf Term
evalNoSugarT  = prefixFailMsg "evalNoSugarT failed: " $
                repeatR (evalR [])
                  >>> checkConfR
                  >>> confToTermT
                  >>> deleteUnusedLetBindingsR

-- Initial settings: ----------------------------------------------------------

initConf :: Term -> Conf
initConf  = (Map.empty, , [])

-- Infinite list of variable names used for sn.
initEvalEnv :: EvalEnv
initEvalEnv  = EvalEnv { steps = 0, sn = names }

-- Calculate the domains of the heap/stack: -----------------------------------

-- According to M&S paper, this should be the set of variables bound by
-- the heap, we just take the set of all variables all using sieveT.
domHeapT :: ( ExtendPath c Crumb, ReadPath c Crumb
            , MonadCatch m, AddBinders c, Walker c U )
            => Transform c m Heap [Name]
domHeapT  = prefixFailMsg "domHeapT failed: " $
            transform $ \c h -> do
              (bs, ts) <- applyT (unzip . Map.toList <$> idR) c h
              ns <- applyT (concatMapT sieveT) c ts
              return (bs ++ ns)

-- Set of update markers constitutes the domain of the stack.
domStack :: Stack -> [Name]
domStack  = catMaybes . fmap (\case
              Update ns   -> Just ns
              _           -> Nothing)

-- Domain of configuration = domain of heap + domain of stack.
domConfT :: ( ExtendPath c Crumb, ReadPath c Crumb
            , MonadCatch m, AddBinders c, Walker c U )
            => Transform c m Conf [Name]
domConfT  = prefixFailMsg "domConfT failed: " $
            transform $ \c (h, t, s) -> do
              hns <- applyT domHeapT c h
              tns <- applyT sieveT c t
              let sns = domStack s
              return (hns ++ tns ++ sns)

-- Evaluator: -----------------------------------------------------------------
{-
  - Implementation of Sestoft's mark 1 abstract machine;
  - See paper for details of each transition rule;
  - We pass in a list of names to be avoided when generating safe names during
    beta-reduction. This is necessary for when we force heap evaluation when
    sugaring syntax;
  - Important note: we only increase evaluation cost for LOOKUP transitions.
-}
evalR :: ( SafeNames s, ExtendPath c Crumb, ReadPath c Crumb
         , AddBinders c, MonadState s m, TimeCost s, MonadCatch m )
         => [Name] -> Rewrite c m Conf
evalR invalid = prefixFailMsg "evalR failed: "
                $ transform $ \c conf -> case conf of
 (h, Var ns, s) -> case Map.lookup ns h of                                       -- { LOOKUP }
   Just t  -> modify incCost >> return (Map.delete ns h, t, Update ns : s)
   Nothing -> fail ("free variable occurrence: '" ++ ns ++ "'.")
 (h, t, (Update ns : s)) | isVal t -> return (Map.insert ns t h, t, s)           -- { UPDATE }
 (h, App t (Var ns), s) -> return (h, t, Variable ns : s)                        -- { UNWIND }
 (h, Abs ns t, Variable ns' : s) -> do                                           -- { SUBST  }
   t' <- applyR (substTryR ns (Var ns')) c t
   return (h, t', s)
 (h, Case t alts, s) -> return (h, t, Alts alts : s)                             -- { CASE   }
 (h, t, Alts alts : s) | isDatatype t -> do                                      -- { BRANCH }
   t' <- applyR substCaseR c ((Case t alts))
   return (h, t', s)
 conf@(h, tel@Let{}, s) -> do                                                    -- { LETREC }
   used <- applyT domConfT c conf
   (Let bs body) <- applyR (safeBindersR $ used ++ invalid) c tel
   return (addBindings h bs, body, s)

 -- Edge cases for errors.
 (_, t, s) | isVal t && null s -> fail "nothing to evaluate."                    -- { VALUE  }
 conf -> fail $ "error state:\n " ++ show conf                                   -- { STATE  }
 where
   addBindings :: Heap -> [Bind] -> Heap
   addBindings  = foldr (\(Bind ns t _) h -> Map.insert ns t h)

-- Evaluate the heap: ---------------------------------------------------------

{-
  - Evaluate all bindings on the heap to value form to allow for syntax
    sugaring;
  - Important note: this is not lazy evaluation because we're forcing
    evaluation, similar to Haskell's deepseq.
-}
evalHeapR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
             , MonadCatch m, MonadState s m, SafeNames s, TimeCost s )
             => Rewrite c m Conf
evalHeapR  = prefixFailMsg "evalHeapR failed: " $
             rewrite $ \c (h, t, s) -> do
 guardMsg (null s) "stack not empty." -- If the stack isn't empty, previous
                                      -- eval error.
 let (vals, evals) = partition (\(_, t) -> isVal t) (Map.toList h)
 if null evals -- evals = non-value bindings
    then return (h, t, s)
    else do
     let (bs, eval) = head evals
     -- Here we need to pass in the binder of the binding to be evaluated in
     -- order to prevent it being generated as a safe name during
     -- beta-reduction.
     (c', (h', t', s')) <- applyT (repeatR (evalR [bs]) >>> exposeT) c
      (Map.fromList (vals ++ tail evals), eval, s)
     -- Add the binding back and recurse.
     applyR evalHeapR c' (Map.insert bs t' h', t, s')

-- Helpers: -------------------------------------------------------------------

-- Check the final configuration of abstract machine to make sure evaluated to
-- value and stack is empty.
checkConfR :: ( SafeNames s, ExtendPath c Crumb, ReadPath c Crumb
              , AddBinders c, MonadState s m, TimeCost s, MonadCatch m )
              => Rewrite c m Conf
checkConfR  = prefixFailMsg "checkConfR failed: " $
              do
                (_, t, s) <- idR
                if isVal t && null s
                   then idR
                   else evalR [] -- If not, force error.

-- Convert abstract machine configuration to a let term.
confToTermT :: MonadCatch m => Transform c m Conf Term
confToTermT  = prefixFailMsg "confToTermT failed: " $
               do
                 (h, t, s) <- idR
                 guardMsg (null s) "stack not empty."
                 return $ Let (extractBindings h) t

-- Extract let bindings from the heap.
extractBindings  :: Heap -> [Bind]
extractBindings h = fmap (\((ns, t), idx) -> Bind ns t idx)
                     $ zip (Map.toList h) [0..]
