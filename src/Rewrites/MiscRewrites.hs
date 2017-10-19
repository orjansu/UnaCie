{-# LANGUAGE LambdaCase #-}

module MiscRewrites
 ( rotateBindingsCClockwiseR -- Rotate let bindings counterclockwise.
 , rotateBindingsClockwiseR  -- Rotate let bindings clockwise.
 ) where

import CtxAST   (Ctx(..))
import CtxUtils (reindexBinds)

import Language.KURE (MonadCatch, Rewrite, contextfreeT, prefixFailMsg)

{-
  Information:
  -----------------------------------------------------------------------------
  - Miscellaneous KURE rewrites.
-}

-- Misc. rewrites for let bindings: -------------------------------------------

-- let { x = M, y = N } in O == let { y = N, x = M } in O
rotateBindingsClockwiseR :: MonadCatch m => Rewrite c m Ctx
rotateBindingsClockwiseR
 = prefixFailMsg "rotate-bindings-clockwise failed: " $
   contextfreeT $ \case
     Let (b : bs) body -> return $ Let (reindexBinds $ bs ++ [b]) body
     _                 -> fail "not a Let."

-- let { x = M, y = N } in O == let { y = N, x = M } in O
rotateBindingsCClockwiseR :: MonadCatch m => Rewrite c m Ctx
rotateBindingsCClockwiseR
 = prefixFailMsg "rotate-bindings-cclockwise failed: " $
   contextfreeT $ \case
     Let bs body -> return $ Let (reindexBinds $ last bs : init bs) body
     _           -> fail "not a Let."