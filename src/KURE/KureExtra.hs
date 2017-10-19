{-# LANGUAGE FlexibleContexts #-}

module KureExtra
  ( any0tdR           -- anytdR from KURE extended to 'zero or more'.
  , applyAtSnocPathR  -- Apply a rewrite at a SnocPath.
  , applyAtSnocPathT  -- Apply a transformation at a SnocPath.
  , concatMapT        -- ConcatMap for transformations.
  , failT             -- Alaways failing transformation.
  , liftR             -- Lift a function to be a rewrite.
  , liftT             -- Lift a function to be a transformation.
  ) where

import Classes   (AddBinders(..))
import Crumb     (Crumb(..))
import Kure      ()
import Universes (U(..))
import Utils     ((.*), concatMapM)

import Control.Arrow (arr)
import Language.KURE
  ( ExtendPath
  , MonadCatch
  , ReadPath
  , Rewrite
  , SnocPath(..)
  , Transform
  , Walker
  , anytdR
  , applyT
  , pathR
  , pathT
  , prefixFailMsg
  , snocPathToPath
  , transform
  , tryR
  )

{-
  Information:
  -----------------------------------------------------------------------------
  - Some functions/transformations that I think would make good additions
    to the KURE library.

  Working notes:
  -----------------------------------------------------------------------------
  - I'd like all the traversals to have a "zero or more" option.
-}

instance Foldable SnocPath where 
  foldr f base (SnocPath p) = foldr f base p
  
-- Lift functions into Transform/Rewrite: -------------------------------------
-- These are fairly trivial but I think the names are useful for in 
-- code documentation.

liftT :: Monad m => (a -> b) -> Transform c m a b 
liftT  = arr

liftR :: Monad m => (a -> a) -> Rewrite c m a
liftR  = arr

-- SnocPath transformations: --------------------------------------------------

-- Apply a transformation at a SnocPath
applyAtSnocPathT :: ( MonadCatch m, ReadPath c Crumb 
                    , ExtendPath c Crumb, AddBinders c ) 
                    => SnocPath Crumb 
                    -> Transform c m U b 
                    -> Transform c m U b
applyAtSnocPathT  = prefixFailMsg "applyAtSnocPathT failed: " 
                    .* pathT 
                    . snocPathToPath

-- Apply a rewrite at a SnocPath
applyAtSnocPathR :: ( MonadCatch m, ReadPath c Crumb 
                    , ExtendPath c Crumb, AddBinders c ) 
                    => SnocPath Crumb 
                    -> Rewrite c m U 
                    -> Rewrite c m U
applyAtSnocPathR  = prefixFailMsg "applyAtSnocPathR failed: "
                     .* pathR 
                     . snocPathToPath

-- Misc.: ---------------------------------------------------------------------

-- Always failing transformation
failT :: Monad m => String -> Transform c m a b 
failT  = fail 

-- concatMap a transformation over a list
concatMapT  :: MonadCatch m => Transform c m a [b] -> Transform c m [a] [b]
concatMapT t = prefixFailMsg "concatMapT failed: " $ 
                transform (concatMapM . applyT t)

-- Zero or more anytdR
any0tdR :: (Walker c a, MonadCatch m) => Rewrite c m a -> Rewrite c m a
any0tdR  = prefixFailMsg "any0tdr failed: " . tryR . anytdR