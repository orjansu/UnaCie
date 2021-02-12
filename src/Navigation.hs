{-# LANGUAGE FlexibleContexts #-}

module Navigation
  ( Direction      -- Datatype for specifying navigation directions.
  , navigateU      -- Navigate around a U.
  , navigateUT     -- Transformation for navigating around a U.
  , genValidPathUT -- Transformation for generating a valid path for a U.
  , genValidPathT  -- Transformation for generating a valid path for
                   -- an injectable.
  , strToDir       -- Convert a string to a direction.
  ) where

import Classes   (AddBinders(..))
import Crumb     (Crumb(..))
import CtxAST    (Alt(..), Bind(..), Ctx(..), GBind(..))
import Kure      ()
import KureExtra (applyAtSnocPathT)
import Universes (U(..))
import Utils     (prepStr, safeTailSnocPath)

import Language.KURE
  ( AbsolutePath
  , ExtendPath
  , MonadCatch
  , ReadPath
  , SnocPath(..)
  , Transform
  , (@@)
  , constT
  , contextfreeT
  , extractT
  , guardMsgM
  , ifM
  , prefixFailMsg
  , snocPathToPath
  , testPathT
 )

{-
  <TO-DO>: - Might be a better way of making sure NEXT navs don't return
             invalid paths, e.g., by storing the number of bindings of each
             let statement. This could be a job for congruence combinators;
           - Add constructor specific navigation commands e.g., LetBody,
             AbsBody etc.;
           - Add a higher level navigation mechanism, like HERMIT's consider.

  Information:
  -----------------------------------------------------------------------------
  - Navigation mechanism for changing the focussed term in the AST during
    transformations;
  - A path of crumbs from the root note dicates which (sub-)term is currently
    under focus. Adding/removing crumbs from this path changes the focus.
  - Recall that transformation rules are applied to the focussed (sub-)term.
-}

{-
  - Currently we have a basic means of navigation, which is to select e.g.,
    the left/right child of a node or going up in/to the top of the AST;
  - Next/prev are for let bindings and case alts;
  - RHS is to access the right-hand side of a binding/alternative.
-}
data Direction = TOP
               | UP
               | LEFT
               | RIGHT
               | NEXT
               | PREV
               | RHS
                 deriving Eq

instance Show Direction where
  show TOP   = "top"
  show UP    = "up"
  show LEFT  = "left"
  show RIGHT = "right"
  show NEXT  = "next"
  show PREV  = "prev"
  show RHS   = "rhs"

-- Top-level function for navigation: -----------------------------------------

navigateU :: Direction -> AbsolutePath Crumb -> U -> Maybe (AbsolutePath Crumb)
navigateU dir p u = nav u >>= \c -> case (c, u) of
  -- Special cases for binds/alts because sometimes we
  -- need to swap the last crumb, not extend the path.
  (Let_Bind{}, UBind{}) -> swapLastCrumb c p
  (Case_Alt{}, UAlt{})  -> swapLastCrumb c p
  _                     -> Just (p @@ c)
  where
   nav (UGBind g) = navigateIntoGBind dir g
   nav (UCtx c)   = navigateIntoCtx   dir c
   nav (UBind b)  = navigateIntoBind  dir b
   nav (UAlt a)   = navigateIntoAlt   dir a


-- Transformation associated with the above function: --

-- We need to check if the resulting path is valid due to navigating NEXT on
-- bindings/alternatives. We assume path p is valid for U.
navigateUT :: ( MonadCatch m, ReadPath c Crumb
              , ExtendPath c Crumb, AddBinders c )
              => Direction
              -> AbsolutePath Crumb
              -> Transform c m U (AbsolutePath Crumb)
navigateUT TOP p = constT $ if p == emptySnocPath
                            then fail "navigateUT failed: already at the top!"
                            else return mempty
navigateUT UP  p = constT $ if p == emptySnocPath
                            then fail "navigateUT failed: you're at the top!"
                            else return $ safeTailSnocPath p
navigateUT dir p = prefixFailMsg "navigateUT failed: " $
                   do
                     -- We navigate at the /end/ of the path parameter
                     p' <- applyAtSnocPathT p (contextfreeT
                            (maybe (fail "navigation not possible.")
                            return . navigateU dir p))
                     -- We then test the new path to make sure it's valid
                     guardMsgM (testPathT (snocPathToPath p'))
                      "navigation not possible."
                     return p'

-- Low-level crumb operations that generate the new navigation path: ----------

-- We only consider navigating 'into' a node, because navigating 'out of'
-- a node (e.g., using up/top) just involves removing crumbs from the path.

navigateIntoGBind :: Direction -> GBind -> Maybe Crumb
navigateIntoGBind RIGHT CBind{} = Just CBind_Body
navigateIntoGBind RIGHT TBind{} = Just TBind_Body
navigateIntoGBind _     _       = Nothing

navigateIntoCtx :: Direction -> Ctx -> Maybe Crumb
navigateIntoCtx RIGHT Abs{}             = Just Abs_Body
navigateIntoCtx LEFT  App{}             = Just App_Fun
navigateIntoCtx RIGHT App{}             = Just App_Arg
navigateIntoCtx RIGHT Tick{}            = Just Tick_Body
navigateIntoCtx LEFT  Let{}             = Just (Let_Bind 0)
navigateIntoCtx RIGHT Let{}             = Just Let_Body
navigateIntoCtx LEFT  Case{}            = Just Case_Scrut
navigateIntoCtx RIGHT Case{}            = Just (Case_Alt 0)
navigateIntoCtx RIGHT (CVar _ _ Just{}) = Just CVar_Body
navigateIntoCtx _     _                 = Nothing

-- We specifically record indices in binds/alts to be used for navigation
-- purposes.
navigateIntoBind :: Direction -> Bind -> Maybe Crumb
navigateIntoBind RHS  (Bind _ _ idx) = Just (Bind_Body idx)
navigateIntoBind NEXT (Bind _ _ idx) = Just (Let_Bind $ idx + 1)
navigateIntoBind PREV (Bind _ _ idx) | idx > 0 = Just (Let_Bind $ idx - 1)
navigateIntoBind _    _              = Nothing

navigateIntoAlt :: Direction -> Alt -> Maybe Crumb
navigateIntoAlt RHS  (Alt _ _ _ idx) = Just (Alt_Body idx)
navigateIntoAlt NEXT (Alt _ _ _ idx) = Just (Case_Alt $ idx + 1)
navigateIntoAlt PREV (Alt _ _ _ idx) |  idx > 0 = Just (Case_Alt $ idx - 1)
navigateIntoAlt _    _               = Nothing

-- Strip crumbs from a path until it becomes valid: ---------------------------

-- These are used as a fail-safe if navigation goes wrong, which it /should/
-- never do.
genValidPathT :: ( MonadCatch m, ReadPath c Crumb
                  , ExtendPath c Crumb, AddBinders c )
                 => AbsolutePath Crumb
                 -> Transform c m U (AbsolutePath Crumb)
genValidPathT  = prefixFailMsg "genValidPathT failed: "
                  . extractT
                  . genValidPathUT

genValidPathUT :: ( MonadCatch m, ReadPath c Crumb
                  , ExtendPath c Crumb, AddBinders c )
                  => AbsolutePath Crumb
                  -> Transform c m U (AbsolutePath Crumb)
genValidPathUT  = prefixFailMsg "genValidPathUT failed: " . go
  where
    go p@(SnocPath [])       = return p
    go p@(SnocPath (_ : cs)) = ifM (testPathT $ snocPathToPath p)
                                   (return p)
                                   (go $ SnocPath cs)

-- Helpers: -------------------------------------------------------------------

emptySnocPath :: SnocPath Crumb
emptySnocPath  = mempty

swapLastCrumb :: Crumb -> SnocPath Crumb -> Maybe (SnocPath Crumb)
swapLastCrumb _ (SnocPath [])       = Nothing
swapLastCrumb c (SnocPath (_ : cs)) = Just (SnocPath $ c : cs)

-- Used for parsing navigation commands entered on the command line.
strToDir :: String -> Maybe Direction
strToDir s = case prepStr s of
  "top"   -> Just TOP
  "up"    -> Just UP
  "left"  -> Just LEFT
  "right" -> Just RIGHT
  "next"  -> Just NEXT
  "prev"  -> Just PREV
  "rhs"   -> Just RHS
  _       -> Nothing
