
module CtxCheck
 ( autoAppBinderChain     -- Form a chain of binder substs. for app. contexts.
 , autoAppCtxBinderSubst  -- Generate applicative contexts using let binders.
 , checkCtxKind           -- Check the kind of a single context.
 , checkCtxsKinds         -- Check the kind of a list of contexts.
 , isAppCtx               -- Check if a context is an applicative kind.
 , isEvalCtx              -- Check if a context is an evaluation kind.
 , isStdCtx               -- Check if a context is an standard kind.
 , isValCtx               -- Check if a context is an value kind.
 ) where

import CtxAST      (Alt(..), Bind(..), Ctx(..), Name)
import CtxEqLib    (CtxEqLib(..))
import CtxKind     (CtxKind(..))
import CtxPatAST   (CtxPat)
import CtxPatMatch (eqCtxPat)
import CtxUtils    (isTerm, bindBinder, isDatatype, isVar)
import Utils       (divide)

import Control.Monad.Extra  (allM)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.List            (partition)
import Data.Maybe           (catMaybes)

{-
  <TO-DO>: Can we use strongly connected components here?

  Information:
  -----------------------------------------------------------------------------
  - Here we check contexts are of a specific /kind/;
  - We use reader to pass around a library of cost-equivalent context
    patterns. These patterns are specified by the user and may not
    (strictly speaking) be contexts of a specific kind, but are
    cost-equivalent;
  - In the paper we call context kinds, context 'forms' ( Graham didn't like
    the word kind (: );
  - See the paper for details of each context kind/form.
-}

-------------------------------------------------------------------------------
-- Top-level functions for context checking: --
-------------------------------------------------------------------------------

-- Check if a context is of a given kind.
checkCtxKind :: CtxEqLib -> Ctx -> CtxKind -> Bool
checkCtxKind lib ctx k = checkCtxsKinds lib [ctx] [k]

-- As above but for a list of contexts;
-- Used when verifying context nestings.
checkCtxsKinds :: CtxEqLib -> [Ctx] -> [CtxKind] -> Bool
checkCtxsKinds _ [] _  = False
checkCtxsKinds lib ctxs ks
  | length ctxs == length ks = and (runReader seq lib)
    where
      seq       = sequence (fmap (\(fun, inp) -> fun inp) checks)
      checks    = zip (fmap kToF ks) ctxs
      kToF STD  = isStdCtx
      kToF VAL  = isValCtx
      kToF EVAL = isEvalCtx
      kToF APP  = isAppCtx
checkCtxsKinds _ _ _ = False

-------------------------------------------------------------------------------
-- Context checking: --
-------------------------------------------------------------------------------
-- Note that throughout we take into consideration syntactic sugar.

-- Standard contexts: ---------------------------------------------------------

isStdCtx ::  Ctx -> Reader CtxEqLib Bool
isStdCtx ctx = (||) <$> isStdCtx' ctx <*> isCtxEq ctx std


isStdCtx' :: Ctx -> Reader CtxEqLib Bool
isStdCtx' Var{}    = return True
isStdCtx' LitInt{} = return True
isStdCtx' LitStr{} = return True
isStdCtx' Hole     = return True
isStdCtx' (Abs _ ctx)     = isStdCtx ctx
isStdCtx' (App ctx Var{}) = isStdCtx ctx

-- C1 C2 is a syntactic sugar for let x = C2 in C1 x
isStdCtx' (App c1 c2) = (&&) <$> isStdCtx c1 <*> isStdCtx c2

-- `C is a syntactic sugar for let x = C in x
isStdCtx' (Tick ctx)    = isStdCtx ctx
isStdCtx' (Let bs ctx)  = (&&) <$> allM (\(Bind _ ctx _) ->
                           isStdCtx ctx) bs <*> isStdCtx ctx
isStdCtx' (Case ctx as) = (&&) <$> isStdCtx ctx
                               <*> allM (\(Alt _ _ ctx _) -> isStdCtx ctx) as

-- Okay so long as it's a valid data type.
isStdCtx' ctx@AppD{} = return (isDatatype ctx)

-- Context kind doesn't matter here as standard context encompass all other
-- kinds.
isStdCtx' (CVar _ _ Nothing) = return True

-- The substitution replaces the hole constructor(s) so
-- better also be a standard context.
isStdCtx' (CVar _ _ (Just ctx)) = isStdCtx ctx

-- Value contexts: ------------------------------------------------------------

isValCtx :: Ctx -> Reader CtxEqLib Bool
isValCtx ctx = (||) <$> isValCtx' ctx <*> isCtxEq ctx val

isValCtx' :: Ctx -> Reader CtxEqLib Bool
isValCtx' LitInt{} = return True
isValCtx' LitStr{} = return True

-- Okay so long as it's a valid data type.
isValCtx' ctx@AppD{} = return (isDatatype ctx)
isValCtx' (Abs _ ctx) = isStdCtx ctx
isValCtx' (CVar VAL _ Nothing) = return True
-- The substitution replaces the hole constructor(s) so
-- better be a /standard/ context
isValCtx' (CVar VAL _ (Just ctx)) = isStdCtx ctx
isValCtx' _ = return False

-- Evaluation contexts: -------------------------------------------------------

isEvalCtx :: Ctx -> Reader CtxEqLib Bool
isEvalCtx ctx = or <$> sequence [ isEvalCtx' ctx
                                , isAppCtx ctx
                                , isCtxEq ctx eval ]

isEvalCtx' ::  Ctx -> Reader CtxEqLib Bool
-- `A is a syntactic sugar for let x = A in x.
isEvalCtx' (Tick ctx) = isAppCtx ctx
-- A M, M /= var, is a syntactic sugar for let x = M in A x.
isEvalCtx' (App c1 c2) | not (isVar c2) = (&& isTerm c2) <$> isAppCtx c1

-- This is the most complicated syntactic form to check, we defer some
-- work to a helper function: isLetEvalCtx.
isEvalCtx' tel@(Let bs ctx) =
  -- let { x_ = M_ } in A
  (||) <$> ((all (\(Bind _ ctx _) -> isTerm ctx) bs &&) <$> isAppCtx ctx)
  -- let { y = M, x_0 = A_0[x_1], x_1 = A_1[x_2], .., x_n = A_n } in A[x_0].
       <*> isLetEvalCtx tel

isEvalCtx' (CVar EVAL _ Nothing) = return True
-- The substitution replaces the hole constructor(s) so
-- better be an /applicative/ context.
isEvalCtx' (CVar EVAL _ (Just ctx)) = isAppCtx ctx
isEvalCtx' _ = return False

-- Applicative contexts: ------------------------------------------------------

isAppCtx  :: Ctx -> Reader CtxEqLib Bool
isAppCtx ctx = (||) <$> isAppCtx' ctx <*> isCtxEq ctx app

isAppCtx' :: Ctx -> Reader CtxEqLib Bool
isAppCtx' Hole = return True
isAppCtx' (App ctx Var{}) = isAppCtx ctx
isAppCtx' (Case ctx as) = (&& all (\(Alt _ _ ctx _) -> isTerm ctx) as)
                           <$> isAppCtx ctx
isAppCtx' (CVar APP _ Nothing) = return True
-- The substitution replaces the hole constructor(s) so
-- better also be an /applicative/ context.
isAppCtx' (CVar APP _ (Just ctx)) = isAppCtx ctx
isAppCtx' _ = return False

-------------------------------------------------------------------------------
-- Helper functions: --
-------------------------------------------------------------------------------

-- Check if a context is cost-equivalent (i.e., in the CtxEqLib).
isCtxEq :: Ctx -> (CtxEqLib -> [CtxPat]) -> Reader CtxEqLib Bool
isCtxEq ctx f = ask >>= return . any (flip eqCtxPat ctx) . f

-- Helpers for complex let evaluation context: --------------------------------
{-
  Special case for evaluation context of the form:
  let { y = M, x_0 = A_0[x_1], x_1 = A_1[x_2], .., x_n = A_n } in A[x_0].
-}
isLetEvalCtx :: Ctx -> Reader CtxEqLib Bool
isLetEvalCtx (Let bs ctx) =
  or <$> sequence [ let Bind _ body _ = bs !! idx
                    in isAppCtx body | idx <- validIdxs ]
  where
   -- (4) Calculate the idxs of the applicative contexts to be checked.
   validIdxs = case ctxSubst of
     Nothing -> []
     Just vs -> catMaybes $ fmap (\subChain ->
                 autoAppBinderChain vs subChain) bssSubst

   -- (3) Calculate which binder has been subbed into the let's
   --     ctx: x_0 in A[x_0].
   ctxSubst = autoAppCtxBinderSubst (fmap bindBinder bs) ctx

   -- (2) Calculate all possible binding chains.
   bssSubst = fmap (\(_, bs) -> (fmap (bSubst (fmap bindBinder bs)) bs)) validParts
   bSubst bs (Bind vs ctx idx) = (vs, autoAppCtxBinderSubst bs ctx, idx)

   -- (1) Divide bs into term/ctx bindings in all possible ways and check
   --     that the terms are all valid.
   validParts = filter (all (\(Bind _ ctx _) -> isTerm ctx) . fst) part
   part = divide bs
isLetEvalCtx _ = return False

{-
  Given

     let { y = M, x_0 = A_0[x_1], x_1 = A_1[x_2], .., x_n = A_n } in A[x_0]

  this is a special case for generating applicative contexts where we return
  the substituted context iff it is a let's binder. This is used to verify let
  bindings for evaluation contexts. Note: there's no need to nest here
  because given terms must be of the form A_i[x_j] for a binder x_j.
-}
autoAppCtxBinderSubst :: [Name] -> Ctx -> Maybe Name
autoAppCtxBinderSubst nss (Var ns)     | ns `elem` nss = Just ns
autoAppCtxBinderSubst nss (App ctx _)  = autoAppCtxBinderSubst nss ctx
autoAppCtxBinderSubst nss (Case ctx _) = autoAppCtxBinderSubst nss ctx
autoAppCtxBinderSubst nss (CVar APP _ (Just ctx)) = autoAppCtxBinderSubst nss ctx
autoAppCtxBinderSubst _ _ = Nothing

{-
  Given:

     let { y = M, x_0 = A_0[x_1], x_1 = A_1[x_2], .., x_n = A_n } in A[x_0]

  We need to form a chain of substitutions. The last element of the chain
  is the context we need to generate:

  x_0 -> x_1 -> x_2 -> .. -> x_n = A_n, and then generate A_n

  Here we calculate the idx of x_n, starting at x_0 and following
  the links. If the links don't form a valid chain ending in x_n,
  then we fail.
-}
autoAppBinderChain :: Name -> [(Name, Maybe Name, Int)] -> Maybe Int
autoAppBinderChain _ [] = Nothing
autoAppBinderChain link chain =
  case partition (\(ns, _, _) -> link == ns) chain of
    ([(_, _, idx)], [])        -> Just idx
    ([(_, Just ns, _)], chain) -> autoAppBinderChain ns chain
    _                          -> Nothing
