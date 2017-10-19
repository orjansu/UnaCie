{-# LANGUAGE LambdaCase #-}

module ListRewrites
 ( appendAssoc_LR_R  -- Re-associate append from left to right.
 , appendAssoc_RL_R  -- Re-associate append from right to left.
 , appendIdentR      -- Evaluate append's left identity.
 ) where

import CtxAST   (Ctx(..), Term)
import CtxUtils ({-isList,-} nil)

import Language.KURE ( MonadCatch, Rewrite
                     , contextfreeT, prefixFailMsg )

{-
  Information:
  -----------------------------------------------------------------------------
  - List rewrites from Moran and Sands' paper.
-}

appendFun :: Term
appendFun  = Var "(++)"

-- Re-associating append from left to right is an improvement: 
-- (xs ++ ys) ++ xs ~> xs ++ (ys ++ zs).
appendAssoc_LR_R :: MonadCatch m => Rewrite c m Term
appendAssoc_LR_R
 = prefixFailMsg "append-assoc-lr failed: " $
   contextfreeT $ \case
    App (App (t1) (App (App t2 xs) ys)) zs
      | and [ t1 == appendFun
            , t2 == appendFun 
           --  , isList xs      
           --  , isList ys      
           --  , isList zs 
            ] -> return $ App (App t1 xs) (App (App t2 ys) zs)
    _  -> fail "incorrect form."

-- Re-associating append from right to left is a weak improvement: 
-- xs ++ (ys ++ zs) ~~> (xs ++ ys) ++ zs.
appendAssoc_RL_R :: MonadCatch m => Rewrite c m Term
appendAssoc_RL_R
 = prefixFailMsg "append-assoc-lr failed: " $
   contextfreeT $ \case
     App (App t1 xs) (App (App t2 ys) zs)
      | and [ t1 == appendFun
            , t2 == appendFun 
          --  , isList xs      
          --  , isList ys      
          --  , isList zs 
            ] -> return $ App (App t1 (App (App t2 xs) ys)) zs
     _ -> fail "incorrect form."

-- Evaluating append's left identity is an improvement: 
-- [] ++ xs ~> xs.
appendIdentR :: MonadCatch m => Rewrite c m Term
appendIdentR
 = prefixFailMsg "append-ident failed: " $
   contextfreeT $ \case
     App (App t1 t2) xs | t1 == appendFun
                       && t2 == nil
                     {- && isList xs-} -> return xs
     _ -> fail "incorrect form."