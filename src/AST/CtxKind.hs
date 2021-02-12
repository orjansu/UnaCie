
module CtxKind
  ( CtxKind(..)    -- Context kind datatype.
  , strToCtxKind   -- Convert a string to a context kind.
  , ctxKindToChar  -- Convert a context kind to a character.
  ) where

import Utils (prepStr)

{-
  Information:
  -----------------------------------------------------------------------------
  - Datatype to capture the different 'kinds' of contexts;
  - The kind of a context dictates its syntactic form, see paper for more
    details.
-}

data CtxKind = STD   -- Standard
             | VAL   -- Value
             | EVAL  -- Evaluation
             | APP   -- Applicative
               deriving (Eq, Enum, Bounded)

instance Show CtxKind where
  show STD  = "Standard"
  show VAL  = "Value"
  show EVAL = "Evaluation"
  show APP  = "Applicative"

-- Helpers: -------------------------------------------------------------------

-- For parsing context kinds input on the command line as command parameters.
strToCtxKind :: String -> Maybe CtxKind
strToCtxKind s = case prepStr s of
  "std"  -> Just STD
  "val"  -> Just VAL
  "eval" -> Just EVAL
  "app"  -> Just APP
  _      -> Nothing

-- Character representation of a context kind, used for pretty printing.
ctxKindToChar :: CtxKind -> Char
ctxKindToChar STD  = 'C'   -- Std. contexts ranged over by C/D (not S).
ctxKindToChar VAL  = 'V'
ctxKindToChar EVAL = 'E'
ctxKindToChar APP  = 'A'
