{-# LANGUAGE LambdaCase #-}

module Classes
  ( AddBinders(..)    -- Class for manipulating binders stored in a context.
  , SafeNames(..)     -- Class for accessing safe names stored in an environment.
  , TimeCost(..)      -- Class for manipulating time cost in an environment.
  , addLetBinders     -- Add let binders to a context.
  , boundVarsContext  -- Return bound variables stored in a context.
  , freshVar          -- Generate a fresh variable from an environment.
  , isBoundContext    -- Check if a variable is bound in a context.
  , isFreeContext     -- Check if a variable is free in a context.
  ) where

import CtxAST   (Bind, Name)
import CtxUtils (bindBinder)
import Utils    ((.*))

import Control.Monad.State (MonadState(..), gets)
import Data.List ((\\))

{-
  Information:
  -----------------------------------------------------------------------------
  - A number of key classes used throughout different modules.
-}

-------------------------------------------------------------------------------
-- Interacting with bound variables stored in a transformation's context: --
-------------------------------------------------------------------------------

class AddBinders c where
  addBinders    :: [Name] -> c -> c
  getBinders    :: c -> [Name]
  modifyBinders :: ([Name] -> [Name]) -> c -> c
  nullBinders   :: c -> c
  nullBinders    = modifyBinders (const mempty)

-- Helpers: -------------------------------------------------------------------

-- We qualify these function names with 'context' to distinguish them from
-- functions that calculate free/bound variables from terms themselves.

boundVarsContext :: AddBinders c => c -> [Name]
boundVarsContext  = getBinders

isBoundContext :: AddBinders c => Name -> c -> Bool
isBoundContext ns = (ns `elem`) . getBinders

isFreeContext :: AddBinders c => Name -> c -> Bool
isFreeContext  = not .* isBoundContext

-- This is useful for congruence combinators.
addLetBinders :: AddBinders c => [Bind] -> c -> c
addLetBinders  = addBinders . fmap bindBinder

-------------------------------------------------------------------------------
-- Accessing safe names from a transformation environment: --
-------------------------------------------------------------------------------

class SafeNames s where
  putSafes    :: [Name] -> s -> s
  putSafes     = modifySafes . const
  delSafes    :: [Name] -> s -> s
  delSafes     = modifySafes . flip (\\)
  getSafes    :: s -> [Name]
  modifySafes :: ([Name] -> [Name]) -> s -> s
  fresh       :: [Name] -> s -> Maybe (Name, s)

-- Generate a fresh variable name;
-- Invalid names passed as a parameter;
-- In practice, used with an infinite list of fresh variable names, so shouldn't
-- run out.
freshVar :: (MonadState s m, SafeNames s) => [Name] -> m Name
freshVar invalid = gets (fresh invalid) >>= \case
                     Nothing -> error "shouldn't happen: freshName"
                     Just (ns, s) -> put s >> return ns

-------------------------------------------------------------------------------
-- Manipulating time cost in an evaluation environment: --
-------------------------------------------------------------------------------

-- So far we only make use of incCost, but the other functions may be useful
-- in the future.
class TimeCost s where
  incCost  :: s -> s
  decCost  :: s -> s
  nullCost :: s -> s
