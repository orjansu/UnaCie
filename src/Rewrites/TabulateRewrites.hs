{-# LANGUAGE LambdaCase #-}

module TabulateRewrites where

import CtxAST
import KureMonad
import KureContext
import CtxEqLib
import CtxKind
import CtxGen
import Utils

import Language.KURE


{-
  <TO-DO>: - These should be generalised from R.

  Information:
  -----------------------------------------------------------------------------
  -  Rewrites relating to the tabulate example in Worker/Wrapper/Makes it/Faster
-}

addFun :: Term
addFun  = Var "(+)"

-- Evaluating an additions's left identity is cost-equiv.
-- 0 + x <~> x
plusLeftIdentR :: MonadCatch m => Rewrite c m Ctx
plusLeftIdentR = prefixFailMsg "plusLeftIdentR failed: " $
   contextfreeT $ \case
     App (App t1 t2) x | t1  == addFun
                      && t2 == LitInt 0 -> return x
     _ -> fail "incorrect form."

unplusLeftIdentR :: MonadCatch m => Rewrite c m Ctx
unplusLeftIdentR  = prefixFailMsg "unplusLeftIdentR failed: " $
  contextfreeT $ \x -> return $ App (App addFun $ LitInt 0) x

-- Evaluating an additions's right identity is cost-equiv.
-- x + 0 <~> x
plusRightIdentR :: MonadCatch m => Rewrite c m Ctx
plusRightIdentR = prefixFailMsg "plusRightIdentR failed: " $
   contextfreeT $ \case
     App (App t1 x) t2 | t1 == addFun
                      && t2 == LitInt 0 -> return x
     _ -> fail "incorrect form."

unplusRightIdentR :: MonadCatch m => Rewrite c m Ctx
unplusRightIdentR  = prefixFailMsg "unplusRightIdentR failed: " $
  contextfreeT $ \x -> return $ App (App addFun x) $ LitInt 0

-- Associativity of plus: -----------------------------------------------------
-- let { t = x + y } in               let { t = z + y } in
--   let { r = t + z } in C[r]  <~>     let { r = x + t } in C[r]
--
-- Pretty hacky, but no other way to do it really.
plusAssocGenR :: CtxEqLib -> R Ctx
plusAssocGenR lib = prefixFailMsg "plusAssocGenR failed: " $ idR >>= \case
  Let [Bind t1 (App (App add1 x) y) _]
   (Let [Bind r (App (App add2 (Var t2)) z) _] body)
    |    t1 == t2
    && add1 == addFun
    && add2 == addFun -> do
      guardMsgM (notNull <$> (constT $ applyT (genCtxsFvBvDisjoint
        (specSubstGen $ Var r) [STD] lib) emptyKureContext body))
        "no valid standard contexts."
      return $ Let [Bind t1 (App (App add1 z) y) 0]
                (Let [Bind r (App (App add2 x) (Var t2)) 0] body)
  _ -> fail "incorrect form."

unplusAssocGenR :: CtxEqLib -> R Ctx
unplusAssocGenR lib = prefixFailMsg "unplusAssocGenR failed: " $ idR >>= \case
  Let [Bind t1 (App (App add1 z) y) _]
   (Let [Bind r (App (App add2 x) (Var t2)) _] body)
    |    t1 == t2
    && add1 == addFun
    && add2 == addFun -> do
      guardMsgM (notNull <$> (constT $ applyT (genCtxsFvBvDisjoint
        (specSubstGen $ Var r) [STD] lib) emptyKureContext body))
        "no valid standard contexts."
      return $ Let [Bind t1 (App (App add1 x) y) 0]
                (Let [Bind r (App (App add2 (Var t2)) z) 0] body)
  _ -> fail "incorrect form."
