{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KureMonad
 ( KureMEnv(..)   -- KURE-transformation-monad's 'default' environment.
 , R              -- Type of rewrites using TM and KureMEnc.
 , RS             -- Type of rewrites paramaterised over TM's environment.
 , T              -- Type of transformation paramaterised over TM's environment.
 , TM(..)         -- KURE's transformation monad.
 , TS             -- Type of transformations paramaterised over TM's environment.
 , initKureMEnv   -- Initial 'default' environment for TM.
 , sv2KureMEnv    -- Convert a list of safe variable names to an initial default
                  -- TM environment.
 ) where

import Classes     (SafeNames(..))
import CtxAST      (Name)
import KureContext (KureContext)
import Utils       (names)

import Control.Monad       (ap, liftM)
import Control.Monad.State (MonadState(..), State, StateT(..), runState)
import Language.KURE       (MonadCatch(..), Transform)

{-
  Information:
  -----------------------------------------------------------------------------
  - TM = "transformation monad"... which is state + failure;
  - TM is parametric in the state, however, its typical use case is alongside
    the KureMEnv, which stores a list of save variable names that can be used
    for e.g., safe subsitution;
  - When evaluating terms with the abstract machine, a different environment
    is used as we also need to track e.g., number of machine steps.

  Working notes:
  -----------------------------------------------------------------------------
  - Can't use the standard definition for the State monad because the
    'fail' function throws an exception which we don't want, hence the
    newtype wrapper and boilerplate code.
-}

newtype TM s a = TM  { unTM :: State s (Either String a) }
data KureMEnv  = KureMEnv { svs :: [Name] }

-- For any state s: --

type TS s a b  = Transform KureContext (TM s) a b -- "T" for transformation.
type RS s a    = TS s a a                         -- "R" for rewrite.

-- Specialised to KureMEnv: --

type T a b     = Transform KureContext (TM KureMEnv) a b
type R a       = T a a

-- We generalise some transformations such that they depend only on
-- environments that provide safe variable names: KureMEnv is one such
-- environment.

instance SafeNames KureMEnv where
  putSafes nss env  = env { svs = nss }
  getSafes          = svs
  modifySafes f env = env { svs = f (svs env) }
  fresh nss env     = case dropWhile (`elem` nss) (svs env) of
                       []         -> Nothing
                       (xs : xss) -> Just (xs, env { svs = xss } )

-- Initial settings for KureMEnv: ---------------------------------------------

initKureMEnv :: KureMEnv
initKureMEnv  = KureMEnv names

sv2KureMEnv :: [Name] -> KureMEnv
sv2KureMEnv  = KureMEnv

-- Boilerplate: ---------------------------------------------------------------

instance Functor (TM s) where
  fmap = liftM

instance Applicative (TM s) where
  pure  = return
  (<*>) = ap

instance Monad (TM s) where
  return x = TM $ StateT $ \n -> return (Right x, n)
  m >>= f  = TM $ StateT $ \n -> case runState (unTM m) n of
              (Right x, n') -> return $ runState (unTM $ f x) n'
              (Left s, n')  -> return (Left s, n')

  -- We don't raise an exception, we return Left, cf. standard either.
  fail s = TM $ StateT $ \n -> return (Left s, n)

instance MonadState s (TM s) where
  get   = TM $ StateT $ \s -> return (Right s, s)
  put s = TM $ StateT $ const $ return (Right (), s)

-- See KURE paper's for more information on MonadCatch.
instance MonadCatch (TM s) where
  m `catchM` f = TM $ StateT $ \n -> case runState (unTM m) n of
                  (Left s, n')  -> return $ runState (unTM $ f s) n'
                  (Right x, n') -> return (Right x, n')
