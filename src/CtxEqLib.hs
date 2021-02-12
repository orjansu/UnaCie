
module CtxEqLib
  ( CtxEqLib (..)  -- Cost-equivalent contexts datatype.
  , ctxKindToProj  -- Convert a context kind to it's CtxEqLib projection fun.
  , deleteCtxEq    -- Delete a cost-equivalent context by index.
  , deleteCtxEqs   -- Delete cost-equivalent contexts of a specific kind.
  , deleteCtxEqss  -- Delete all cost-equivalent contexts.
  , emptyCtxEqLib  -- Initial CtxEqLib settings.
  , insertCtxEqs   -- Insert a cost-equivalent context.
  ) where

import CtxKind   (CtxKind(..))
import CtxPatAST (CtxPat)
import Utils     (deleteAtIdx)

import Data.List (nub)

{-
  <TO-DO>: - Long term goal is to introduce a mechanism that allows users to
             verify these are cost-equivalent. Then mark them as safe/unsafe
             depending on these verifications. Take inspiration from HERMIT's
             lemmas.

  Information:
  -----------------------------------------------------------------------------
  - A library of cost-equivalent contexts, which are stored as context
    /patterns/ so they can be utilised more generally. Cost-equivalent
    contexts are strictly speaking the wrong syntactic form to be a context of
    any kind, however, they are cost-equivalent to a context of a /specific/
    kind;
  - Additions to this library are made by the user and /no/ validation is
    performed currently. This means it is the users responsibility to ensure
    this library is correct;
  - Important note: abusing this library can lead to /incorrect/ proofs.
-}

-- We store a list of context patterns for each context kind
data CtxEqLib = CtxEqLib
  { std  :: [CtxPat]   -- Standard cost-equivalent contexts
  , val  :: [CtxPat]   -- Value
  , eval :: [CtxPat]   -- Evaluation
  , app  :: [CtxPat]   -- Applicative
  }

-- Initial settings: ----------------------------------------------------------

emptyCtxEqLib :: CtxEqLib
emptyCtxEqLib  = CtxEqLib [] [] [] []

-- Helpers: -------------------------------------------------------------------

-- Convert a context kind to its CtxEqLib projection function
ctxKindToProj :: CtxKind -> (CtxEqLib -> [CtxPat])
ctxKindToProj STD  = std
ctxKindToProj VAL  = val
ctxKindToProj EVAL = eval
ctxKindToProj APP  = app

-- Insert cost-equiv. contexts of a specific kind
insertCtxEqs :: [CtxPat] -> CtxKind -> CtxEqLib -> CtxEqLib
insertCtxEqs pcs k lib = case k of
  STD  -> lib { std  = nub $ std  lib ++ pcs }
  VAL  -> lib { val  = nub $ val  lib ++ pcs }
  EVAL -> lib { eval = nub $ eval lib ++ pcs }
  APP  -> lib { app  = nub $ app  lib ++ pcs }

-- Delete a specific cost-equiv. context of a given kind;
-- Fails if incorrect index.
deleteCtxEq :: CtxKind -> Int -> CtxEqLib -> Maybe CtxEqLib
deleteCtxEq k i lib  = case k of
  STD  -> (fmap (\s -> lib { std  = s }) . deleteAtIdx i) (std  lib)
  VAL  -> (fmap (\v -> lib { val  = v }) . deleteAtIdx i) (val  lib)
  EVAL -> (fmap (\e -> lib { eval = e }) . deleteAtIdx i) (eval lib)
  APP  -> (fmap (\a -> lib { app  = a }) . deleteAtIdx i) (app  lib)

-- Delete all cost-equiv. contexts of a given kind
deleteCtxEqs :: CtxKind -> CtxEqLib -> CtxEqLib
deleteCtxEqs k lib = case k of
  STD  -> lib { std  = [] }
  VAL  -> lib { val  = [] }
  EVAL -> lib { eval = [] }
  APP  -> lib { app  = [] }

-- Delete all cost-equiv. contexts
deleteCtxEqss :: CtxEqLib -> CtxEqLib
deleteCtxEqss  = const emptyCtxEqLib
