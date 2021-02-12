
module NavSettings where

import Crumb         (Crumb)
import Language.KURE (AbsolutePath)

{-
  Information:
  -----------------------------------------------------------------------------
  - Settings to determine whether the focussed term should be displayed
    in isolation (isHigh = False) or highlighted (isHigh = True);
  - We fix the term currently on-screen when highlighting is active. This is
    achieved by storing a path to this (sub-)term in the highPath variable.
-}

data NavSettings = NavSettings { isHigh   :: Bool
                               , highPath :: AbsolutePath Crumb }

-- Initial settings: ----------------------------------------------------------

initNavSettings :: NavSettings
initNavSettings  = NavSettings { isHigh = False, highPath = mempty }

-- Helpers: -------------------------------------------------------------------

isHighlighted :: NavSettings -> Bool
isHighlighted  = isHigh

highlight :: AbsolutePath Crumb -> NavSettings -> NavSettings
highlight p ns = ns { isHigh = True, highPath = p }

unHighlight :: NavSettings -> NavSettings
unHighlight ns = ns { isHigh = False, highPath = mempty }

updateHighPath :: AbsolutePath Crumb -> NavSettings -> NavSettings
updateHighPath p ns = ns { highPath = p }

getHighPath :: NavSettings -> AbsolutePath Crumb
getHighPath  = highPath
