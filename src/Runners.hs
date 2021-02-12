{-# LANGUAGE FlexibleContexts #-}

module Runners
  ( applyRCtx     -- Apply a context rewrite with the initial environment.
  , applyREnvCtx  -- Apply a context rewrite with a given environment.
  , applyREnvU    -- Apply a U rewrite with a given environment.
  , applyT'       -- Apply a transformation with the initial environment.
  , applyTCtx     -- Apply a context transformation with the initial environment.
  , applyTEnvCtx  -- Apply a context transformation with a given environment.
  , applyTEnvU    -- Apply a U transformation with a given environment.
  , applyTU       -- Apply a U transformation with the initial environment.
  , applyTUInj    -- Apply a U transformation to an injetable with the
                  -- initial environment.
  , evalTMEnv     -- Evaluate TM computation with a given environment.
  , runTMEnv      -- Run TM computation with a given environment.
  ) where

import CtxAST      (Ctx)
import KureContext (emptyKureContext)
import KureMonad   (KureMEnv, R, T, TM(..), initKureMEnv)
import Universes   (U(..))

import Control.Monad.State (evalState, runState)
import Language.KURE       (Injection(..), applyR, applyT)

{-
  Information:
  -----------------------------------------------------------------------------
  - Top-level functions for evaluating a range of monadic computations;
  - In general, I call them 'runners' (I think this is a common name?).
-}

-------------------------------------------------------------------------------
-- Runners for TM: --
-------------------------------------------------------------------------------

-- Evaluate TM with a given environment.
evalTMEnv :: TM s a -> s -> Either String a
evalTMEnv m env = evalState (unTM m) env

-- Run TM with a given environment.
runTMEnv :: TM s a -> s -> (Either String a, s)
runTMEnv m env = runState (unTM m) env

-------------------------------------------------------------------------------
-- Runners for T: --
-------------------------------------------------------------------------------

-- Apply T with the initial (default) environment: --

applyTCtx :: T Ctx b -> Ctx -> Either String b
applyTCtx t = (flip evalTMEnv initKureMEnv) . applyT t emptyKureContext

applyTU :: T U b -> U -> Either String b
applyTU t = (flip evalTMEnv initKureMEnv) . applyT t emptyKureContext

applyT' :: T a b -> a -> Either String b
applyT' t  = (flip evalTMEnv initKureMEnv) . applyT t emptyKureContext

applyTUInj :: Injection a U => T U b -> a -> Either String b
applyTUInj t = (flip evalTMEnv initKureMEnv)
                . applyT t emptyKureContext
                . inject

-- Apply T with a given environment: --

applyTEnvCtx  :: T Ctx b -> KureMEnv -> Ctx -> Either String b
applyTEnvCtx t env = (flip evalTMEnv env) . applyT t emptyKureContext

applyTEnvU :: T U b -> KureMEnv -> U -> Either String b
applyTEnvU t env = (flip evalTMEnv env) . applyT t emptyKureContext

-------------------------------------------------------------------------------
-- Runners for R: --
-------------------------------------------------------------------------------

-- Apply R with the initial (default) environment: --

applyRCtx :: R Ctx -> Ctx -> Either String Ctx
applyRCtx r = (flip evalTMEnv initKureMEnv) . applyR r emptyKureContext

-- Apply R with a given environment: --

applyREnvCtx :: R Ctx -> KureMEnv -> Ctx -> Either String Ctx
applyREnvCtx r env = (flip evalTMEnv env) . applyR r emptyKureContext

applyREnvU :: R U -> KureMEnv -> U -> Either String U
applyREnvU r env = (flip evalTMEnv env) . applyR r emptyKureContext
