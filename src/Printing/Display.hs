
module Display (display) where

import Brick.AttrMap      (attrMap)
import Brick.Types        (Widget, ViewportType(..))
import Brick.Widgets.Core ( vBox, viewport, str, padLeft
                          , padTop, padBottom )
import Control.Monad      (void)

import qualified Graphics.Vty as V
import qualified Brick.Types  as T
import qualified Brick.Main   as M

{-
  Information:
  -----------------------------------------------------------------------------
  - Display function for nicely viewing output on screen in a new window.
  - Adapted from the brick examples.
-}

-- This code is adapted from:
-- https://hackage.haskell.org/package/brick-0.28/src/programs/
--  ViewportScrollDemo.hs
data Name = VP deriving (Ord, Show, Eq)

drawUi :: [String] -> () -> [Widget Name]
drawUi ss = const [ padLeft (T.Pad 1)
                    . padTop (T.Pad 1)
                    . padBottom (T.Pad 1)
                    . viewport VP Vertical
                    . vBox
                    $ str <$> ss ]

vpScroll :: M.ViewportScroll Name
vpScroll  = M.viewportScroll VP

appEvent :: () -> T.BrickEvent Name e -> T.EventM Name (T.Next ())
appEvent _ (T.VtyEvent (V.EvKey V.KDown [])) =
  M.vScrollBy vpScroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KUp [])) =
  M.vScrollBy vpScroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt ()
appEvent _ _ = M.continue ()

app :: [String] -> M.App () e Name
app ss = M.App { M.appDraw = (drawUi ss)
              , M.appStartEvent = return
              , M.appHandleEvent = appEvent
              , M.appAttrMap = const $ attrMap V.defAttr []
              , M.appChooseCursor = M.neverShowCursor
              }

-- Display a list of strings on screen.
display :: [String] -> IO ()
display ss = void $ M.defaultMain (app ss) ()
