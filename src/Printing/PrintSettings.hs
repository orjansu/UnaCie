
module PrintSettings
  ( fileLineStyle       -- Style based on fileLineWidth.
  , fileLineWidth       -- Line width for printing to files.
  , terminalLineStyle   -- Style based on terminalLineWidth.
  , terminalLineWidth   -- Line width for displaying output in the terminal.
  ) where 

import Text.PrettyPrint.HughesPJ (Mode(..), Style (..))

{-
  Information:
  ----------------------------------------------------------------------------- 
  - Settings for displaying output to the terminal/when printed to files.
-}

-- Terminal print settings: ---------------------------------------------------

-- Terminal line width of 80 chars
terminalLineWidth :: Int 
terminalLineWidth  = 80

-- Terminal line style based on terminalLineWidth.
terminalLineStyle :: Style 
terminalLineStyle  = Style { mode           = PageMode
                           , lineLength     = terminalLineWidth
                           , ribbonsPerLine = 1 :: Float }

-- File print settings: -------------------------------------------------------

-- File line width of 100 chars.
fileLineWidth :: Int 
fileLineWidth  = 100

-- File line style based on fileLineWidth.
fileLineStyle :: Style 
fileLineStyle  = Style { mode           = PageMode
                       , lineLength     = fileLineWidth
                       , ribbonsPerLine = 1 :: Float }