{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module InterPrintUtils where 

import CmdAST        ( Cmd(..), Param(..), RawCmd(..)
                     , RawParam(..), LocatedRawParam(..)
                     , Script )
import CtxKind       (CtxKind(..))
import Crumb         (Crumb)
import Universes     (U)
import Relations     (relToStr)
import CmdHist       (CmdHist, steps, navSteps)
import TransHist     (TransHist(..), emptyTransHist)
import PPLib         (ppr)
import KureExtra     (applyAtSnocPathT)
import CtxEqLib      (CtxEqLib, ctxKindToProj)
import PrintSettings ( terminalLineStyle, terminalLineWidth
                     , fileLineStyle, fileLineWidth )
import InterEnv      (InterEnv(..), TransEnv(..), BaseLib(..))
import Utils         ((.*), deggar, stripSpace)
import NavSettings   (isHigh, getHighPath, updateHighPath)
import TransUtils    (uToShowUT)
import InterUtils
import PPLib
import CtxShowAST
import Display

import Data.List     (intersperse, transpose, isPrefixOf)
import Data.Maybe    (isJust)
import Language.KURE (SnocPath(..), AbsolutePath, idR)
import Control.Arrow ((>>>))
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ
import Control.Monad.IO.Class    (liftIO)

{-
  <TO-DO>: N/A

  Information:
  -----------------------------------------------------------------------------
  - This module handles all printing for the interactive mode:
    - Printing to the terminal;
    - Printing to files.
  - E.g., adding colours, frames, highlighting, pretty formatting, spacing;
  - The PrintSettings module controls the line width for the terminal/files
    and these print functions adhere (as best as they can) to those settings;
  - Current setting are 80 line width for terminal and 100 for files.
-} 

-------------------------------------------------------------------------------
-- Terminal printing constants/primitives: --
-------------------------------------------------------------------------------

-- Relations: -- 

relWidth        ::  Int 
relWidth         =  2 

relLeftPadding  ::  Int 
relLeftPadding   =  1    -- '  'REL

relRightPadding ::  Int  
relRightPadding  =  1    -- REL''

relPaddedWidth  ::  Int 
relPaddedWidth   =  relWidth 
                     + relLeftPadding
                     + relRightPadding

-- Proof hints: --

hintBorderWidth      ::  Int 
hintBorderWidth       =  1    -- '{'

hintInnerPadding     ::  Int 
hintInnerPadding      =  1    -- {' '    ' '}

innerHintWidth       ::  Int -> Int 
innerHintWidth outer  =  outer
                          - relPaddedWidth
                          - 2 * hintBorderWidth
                          - 2 * hintInnerPadding

-- Character bullets: --

defaultBullet      ::  Char 
defaultBullet       =  '\9702'

spaceBullet        ::  Char 
spaceBullet         =  ' '

bulletWidth        ::  Int 
bulletWidth         =  1 

bulletRightPadding ::  Int 
bulletRightPadding  =  1

-- Numbered bullets: -- 

numberedBulletWidth        ::  Int -> Int 
numberedBulletWidth n       =  n + 2 -- '(X)'

numberedBulletRightPadding ::  Int 
numberedBulletRightPadding  =  1     -- (X)' '

-- Framed output: --

outerFrameWidth          ::  Int 
outerFrameWidth           =  1     -- '|'

outerFramePadding        ::  Int 
outerFramePadding         =  1     -- |' '

terminalInnerFramedWidth ::  Int 
terminalInnerFramedWidth  =  terminalLineWidth 
                              - 2 * outerFrameWidth 
                              - 2 * outerFramePadding

-- Separators: --

terminalLineSep            ::  String 
terminalLineSep             =  replicate terminalLineWidth '-'

terminalInnerFramedLineSep ::  String 
terminalInnerFramedLineSep  =  replicate terminalInnerFramedWidth '-'

fileLineSep                ::  String 
fileLineSep                 =  replicate fileLineWidth '-'

-------------------------------------------------------------------------------
-- Terminal printing: -- 
-------------------------------------------------------------------------------
-- All output to terminal has non-strict line output, see CmdAST for details.

-- Prompts: -------------------------------------------------------------------

{-

-- Transformation state prompt:
--- Shows a sub-term of the term being transformed (using the current path 
--- through the term's AST) and TransHist state
transPrompt  ::  InterM InterEnv (Maybe Int)
transPrompt   
 =  getInterEnv mTransEnv >>= \case
     Just transEnv -> do 

      let t  = getTerm transEnv
          p  = getPath transEnv 
          g  = getGoal transEnv
          hi = (length . stateHist . getHist) transEnv

      -- Check transformation goal and output info message
      -- if reached. <TO-DO> this should be alpha-equiv
      if Just t == g 
         then interPutInfo "transformation goal reached."
         else return ()
 
      -- Print relation of last command
      maybe (return ()) interPrint (getCRel transEnv)

      -- Print sub-term from current location in term's AST
      either (const $ -- \err -> outputCmdError err  -- KURE error is crumb failure
               interPutWarning "invalid path, resetting..." 
               >> interPutStrLn (renderStyle terminalLineStyle $ ppr t)
               >> modifyInterEnv (putTransEnv $ nullPath transEnv)
               >> return (Just hi))
             (\t -> interPutStrLn (renderStyle terminalLineStyle $ ppr t)
               >> return (Just hi))
             (transExec (applyAtSnocPathT p idR) t)

     -- Critical error if we've ended up in the transformation
     -- state /without/ a transEnv being set
     Nothing       -> transEnvCritError >> return Nothing


-}

highlightFocus :: AbsolutePath Crumb -> AbsolutePath Crumb -> U -> ShowSettings
highlightFocus p p' _ | p == p'   = highlightShowSettings
                      | otherwise = defaultShowSettings

vanilla :: AbsolutePath Crumb -> U -> ShowSettings
vanilla _ _ = defaultShowSettings


transPrompt :: InterM InterEnv (Maybe Int)
transPrompt  = getInterEnv mTransEnv >>= \case
  Just transEnv -> do 

   let t  = getTerm transEnv
       p  = getPath transEnv 
       g  = getGoal transEnv
       hi = (length . stateHist . getHist) transEnv
       ns = getNavSettings transEnv 
       resetPath = resetPath' t hi transEnv

   -- Check transformation goal and output info message
   -- if reached. <TO-DO> this should be alpha-equiv
   if Just t == g 
      then interPutInfo "transformation goal reached!"
      else return ()

   -- Print relation of last command
   maybe (return ()) interPrint (getCRel transEnv)

   if isHigh ns 

   -- Navigation frozen: ------------------------------------------------------
      
   then let (SnocPath fp) = getHighPath ns
            (SnocPath cp) = p

        in if (reverse fp) `isPrefixOf` (reverse cp)

   -- User is still in frozen view: -------------------------------------------
            
           then let (highP, projP) = splitAt (length cp - length fp) cp
                in case transExec (applyAtSnocPathT (SnocPath projP) idR 
                     >>> uToShowUT (highlightFocus $ SnocPath highP)) t of 
                     Left _   -> resetPath 
                     Right su -> outputShowU su >> return (Just hi)
                     
   -- User navigated out of frozen view: --------------------------------------

            else case transExec (applyAtSnocPathT p idR 
                  >>> uToShowUT (highlightFocus mempty)) t of
                  Left _   -> resetPath 
                  Right su -> do 
                   interPutWarning "navigated outside of frozen view, \
                                    \updating view..."
                   modifyInterEnv (putTransEnv $ putNavSettings 
                    (updateHighPath p ns) transEnv)
                   outputShowU su
                   return (Just hi)
               
   -- Navigation frozen -------------------------------------------------------

      else case transExec (applyAtSnocPathT p idR >>> uToShowUT vanilla) t of 
            Left _   -> resetPath
            Right su -> outputShowU su >> return (Just hi)

  -- Critical error if we've ended up in the transformation
  -- state /without/ a transEnv being set
  Nothing -> transEnvCritError >> return Nothing
 
 where
  resetPath' t hi transEnv = case transExec (uToShowUT vanilla) t of 
   -- Critical error if we can't print the entire term
   Left _   -> pathCritError >> return Nothing
   Right su -> do 
    interPutWarning "invalid path, resetting..." 
    modifyInterEnv (putTransEnv . unHighlightNav . nullPath $ transEnv)
    outputShowU su
    return (Just hi)

  outputShowU su = interPutStrLn (renderStyle terminalLineStyle 
                    $ ppr (su, fancyKeywords, inherit))


   






           





















-- Script state prompt:
--- Shows name of active script and next command to be executed
--- from the script on <return>
scriptPrompt ::  InterM InterEnv ()
scriptPrompt  =  getInterEnv getActiveScripts >>= \case
                  
                  ((s, ref, i) : _) -> 

                    let cmdWidth  = terminalLineWidth - length prompt - 3 -- 3 = ' --'
                        prompt    = " Active script: "  
                                     ++ chop 10 ref -- only 10 chars of script name 
                                     ++ " | next command (" ++ show i ++ "): "
                    
                    in interPutStrLn $ "\ESC[30m\ESC[3m\ESC[48;5;252m" 
                                        ++ padTo ' ' terminalLineWidth (prompt 
                                        ++ nextCmd s i cmdWidth) 
                                        ++ " \ESC[m " 

                  -- Critical error when in script state
                  -- but no active script
                  []                -> scriptEnvCritError
                 where nextCmd s i cmdWidth 
                        | i >= length s = "<end of script> "
                        | otherwise     = (chop cmdWidth 
                                           . newlinesToSpaces 
                                             -- doesn't matter about render as chopped   
                                           . renderStyle terminalLineStyle
                                           . pprRawCmdNoLocNS) (s !! i)
                    
transScriptPrompt ::  InterM InterEnv (Maybe Int)
transScriptPrompt  =  scriptPrompt >> transPrompt

-- Command history: -----------------------------------------------------------

cmdHistToTerminal  ::  CmdHist -> [String]
cmdHistToTerminal h  
 =  terminalMultiNumberedList' "Command history" 
     $ fmap showCmd $ reverse h
    where  
         showCmd     = lines . renderStyle cmdStyle . pprCmdNS . fst         
         maxNumWidth = length $ show $ length h - 1
         cmdWidth    = terminalInnerFramedWidth 
                        - numberedBulletWidth maxNumWidth
                        - numberedBulletRightPadding
         cmdStyle    = genStyle cmdWidth

-- State history: -------------------------------------------------------------

stateHistToTerminal  ::  TransHist -> [String]
stateHistToTerminal h
 =  (terminalMultiNumberedList' "State history" 
     . fmap (\(t, _) -> showTerm t)
     . reverse) states 
    where 
         showTerm    = lines . renderStyle termStyle . ppr 
         maxNumWidth = length $ show $ length states - 1
         termWidth   = terminalInnerFramedWidth 
                        - numberedBulletWidth maxNumWidth
                        - numberedBulletRightPadding
         termStyle   = genStyle termWidth                      
         states      = stateHist h 

-- Transformation history: ----------------------------------------------------

transHistToTerminal  ::  TransHist -> [String]
transHistToTerminal h  
 =  terminalAddCentreHighlightedTitle "Transformation history" $
     if h == emptyTransHist 
        then ["<empty>"]
        else concatMap (\((c, r), (t, _)) -> showRelCmd r c 
              ++ showTerm t) xs
    where 
         showTerm = lines . renderStyle termStyle . ppr

         showRelCmd r c = (ys ++ xs) : fmap (whitePad relPaddedWidth ++) xss
                          where 
                               ys         = showRel r
                               (xs : xss) = showCmd c 

         showRel Nothing  = whitePad relPaddedWidth
         showRel (Just r) = let sr = show r 
                            in whitePad relLeftPadding
                                ++ whitePad (relWidth - length sr)
                                ++ sr
                                ++ whitePad relRightPadding 

         showCmd cmd = let (f : fs) = deggar $ lines $ renderStyle hintStyle $ pprCmdNS cmd
                           (g : gs) = reverse $ ('{' : whitePad hintInnerPadding ++ f)
                                       : fmap (whitePad (hintInnerPadding + 1) ++) fs
                       in reverse $ (g ++ whitePad hintInnerPadding ++ "}") : gs

         termStyle = genStyle terminalInnerFramedWidth                 
         hintStyle = genStyle (innerHintWidth terminalInnerFramedWidth)                 
         
         xs        = reverse (zip cmds states)
         states    = stateHist h
         cmds      = cmdHist h

-- Map keys: ------------------------------------------------------------------

mapKeysToTerminal       ::  String -> Map.Map String a -> [String]
mapKeysToTerminal title  =  terminalMultiDefaultBulletList title 
                             . fmap (squashString keyWidth) 
                             . Map.keys
                            where keyWidth = terminalInnerFramedWidth 
                                              - bulletWidth
                                              - bulletRightPadding

-- List without bullets, and new style of heading.
mapKeysToTerminal' :: String -> Map.Map String a -> [String]
mapKeysToTerminal' title = terminalMultiSpaceBulletList title 
                            . fmap (squashString keyWidth) 
                            . Map.keys
                           where keyWidth = terminalInnerFramedWidth 
                                             - bulletWidth
                                             - bulletRightPadding
-- Cost-equivalent contexts library: ------------------------------------------

ctxEqLibToTerminal     ::  CtxEqLib -> [[String]]
ctxEqLibToTerminal lib  =  fmap (\k -> terminalMultiNumberedList' 
                            (ctxKindToDescrip k) . render  
                              $ (ctxKindToProj k) lib) 
                           [minBound..maxBound]
                           where 
                                render ctxEqs = let l = length $ show $ length ctxEqs - 1 
                                                in fmap (lines 
                                                          . renderStyle (ctxEqStyle l) 
                                                          . ppr) ctxEqs

                                ctxEqStyle l = genStyle $ terminalInnerFramedWidth 
                                                           - numberedBulletWidth l
                                                           - numberedBulletRightPadding

ctxEqsToTerminal       ::  CtxKind -> CtxEqLib -> [String]
ctxEqsToTerminal k lib  =  terminalMultiNumberedList' (ctxKindToDescrip k) $ 
                            fmap (lines . renderStyle ctxEqStyle . ppr) ctxEqs
                           where 
                                ctxEqs     = ctxKindToProj k lib
                                maxLen     = length $ show $ length ctxEqs - 1 
                                ctxEqStyle = genStyle $ terminalInnerFramedWidth 
                                                         - numberedBulletWidth maxLen
                                                         - numberedBulletRightPadding

-- Script: --------------------------------------------------------------------

-- Non-strict line output
scriptToTerminal :: String -> Script -> [String]
scriptToTerminal ref s = 
  terminalMultiNumberedList' ("Command script: " ++ ref) (terminalRenderScript s)

-- Non-script line output                               
activeScriptToTerminal :: (Script, String, Int) -> [String]
activeScriptToTerminal (s, ref, i) = 
  terminalHighlightedMultiNumberedList ("Active command script: " ++ ref)
    i (terminalRenderScript s)

-- Non-script line output                                       
terminalRenderScript :: Script -> [[String]]
terminalRenderScript s = fmap showCmd s
  where 
    showCmd = fmap (chop cmdWidth) 
               . lines 
               . renderStyle cmdStyle 
               . pprRawCmdNoLocNS

    maxNumWidth = length $ show $ length s - 1
    cmdWidth    = terminalInnerFramedWidth 
                   - numberedBulletWidth maxNumWidth
                   - numberedBulletRightPadding
    cmdStyle    = genStyle cmdWidth

-- Transformation goal: -------------------------------------------------------

goalToTerminal          ::  TransEnv -> [String]
goalToTerminal transEnv  =  terminalAddCentreHighlightedTitle "Transformation goal" $ 
                            case getGoal transEnv of 
                             Nothing -> ["<none>"]
                             Just t  -> lines 
                                         . renderStyle goalStyle 
                                         . ppr $ t     
                            where goalStyle = genStyle terminalInnerFramedWidth  

-- Transformation global relation: --------------------------------------------

grelToTerminal          ::  TransEnv -> [String]
grelToTerminal transEnv  =  terminalAddCentreHighlightedTitle "Global relation" $ 
                            case getGRel transEnv of 
                             Nothing  -> ["<none>"]
                             Just rel -> [show rel]


-------------------------------------------------------------------------------
-- File printing: -- 
-------------------------------------------------------------------------------
-- Some file output has strict line output, see CmdAST for details.

-- State history: -------------------------------------------------------------
                            
stateHistToFile :: TransEnv -> [String]
stateHistToFile transEnv = [""] ++ subHeader ++ hist ++ warn
    where
         histHeader  = "State history: --"
         subHeader   = [fileLineSep, histHeader, fileLineSep]
         hist        = stateHistToFileHelper (getHist transEnv)
         warn        = if gRelSet 
                          then []
                          else fileLineSep : gRelWarn
         gRelWarn    = squashString fileLineWidth "Warning: the global \
                       \relation has not been set, therefore the above \
                       \transformation states do not necessarily \
                       \correspond to an inequational proof."
         gRelSet     = isJust (getGRel transEnv)


-- Not-strict line output
stateHistToFileHelper :: TransHist -> [String]
stateHistToFileHelper h  
 =  concatMap (\((_, r), (t, _)) -> [showRel r] ++ showTerm t ++ [""]) xs
    where 
         showTerm = lines . renderStyle fileLineStyle . ppr

         showRel Nothing  = ""
         showRel (Just r) = show r ++ "\n"
         xs     = reverse (zip cmds states)
         states = stateHist h
         cmds   = cmdHist h 

-- Transformation history: -----------------------------------------------------

transHistToFile :: TransEnv -> [String]
transHistToFile transEnv =  [""] ++ infoBlock  ++ subHeader ++ hist ++ warn
  where 
    infoHeader     = "Transformation info: -----"
    goalHeader     = "-- Goal: -----------------"
    grelHeader     = "-- Global relation: ------"
    histHeader     = "Transformation history: --"
    stepsHeader    = "-- Steps: ---------------"
    navStepsHeader = "-- Navigation steps: ----"
    gRelWarn       = squashString fileLineWidth  "Warning: as the \
                    \global relation has not been set, the above \
                    \transformation steps do not necessarily \
                    \constitute an inequational proof."

    infoBlock  = [ fileLineSep, infoHeader, fileLineSep 
                 , goalHeader, outGoal
                 , grelHeader, outGRel
                 , stepsHeader, show steps
                 , navStepsHeader, show navSteps
                 ]

    subHeader  = [fileLineSep, histHeader, fileLineSep]

    warn       = if gRelSet 
                   then []
                   else fileLineSep : gRelWarn

    hist       = transHistToFileHelper (getHist transEnv)

    outGRel    = case getGRel transEnv of 
                 Nothing  -> "N/A"
                 Just rel -> show rel
    outGoal    = case getGoal transEnv of 
                 Nothing -> "N/A"
                 Just t  -> renderStyle fileLineStyle (ppr t)
              
    gRelSet    = isJust (getGRel transEnv)
    steps      = CmdHist.steps (cmdHist . getHist $ transEnv)
    navSteps   = CmdHist.navSteps (cmdHist . getHist $ transEnv)

-- Non-strict line output
transHistToFileHelper :: TransHist -> [String]
transHistToFileHelper h  =
    concatMap (\((c, r), (t, _)) -> showRelCmd r c ++ [""] ++ showTerm t ++ [""]) xs 
    where 
         showTerm = lines . renderStyle fileLineStyle . ppr

         showRelCmd r c = (ys ++ xs) : fmap (whitePad relPaddedWidth ++) xss
                          where 
                               ys         = showRel r
                               (xs : xss) = showCmd c

         showRel Nothing  = whitePad relPaddedWidth 
         showRel (Just r) = let sr = show r 
                            in whitePad relLeftPadding  
                                ++ whitePad (relWidth - length sr)
                                ++ sr
                                ++ whitePad relRightPadding

         showCmd cmd = let (f : fs) = deggar $ lines $ renderStyle hintStyle $ pprCmdNS cmd
                           (g : gs) = reverse $ ('{' : whitePad hintInnerPadding ++ f)
                                       : fmap (whitePad (hintInnerPadding + 1) ++) fs
                           in reverse $ (g ++ whitePad hintInnerPadding ++ "}") : gs

         hintStyle = genStyle (innerHintWidth fileLineWidth)                      
         xs        = reverse (zip cmds states)
         states    = stateHist h
         cmds      = cmdHist h 

-- Terms/contexts: ------------------------------------------------------------

termsCtxsLibToFile :: Bool -> Bool -> BaseLib -> [String]
termsCtxsLibToFile tb cb lib = case (tb, cb) of 
  (True, True) -> [""] 
                   ++ [fileLineSep, termHeader, fileLineSep]
                   ++ ts
                   ++ [""]
                   ++ [fileLineSep, ctxHeader, fileLineSep]
                   ++ cs 
  (True, False) -> [fileLineSep, termHeader', fileLineSep]
                    ++ ts
  (False, True) -> [fileLineSep, ctxHeader, fileLineSep]
                    ++ cs
  (False, False) -> ["<empty>"]
  where
    termHeader  = "-- Terms: -----"
    termHeader' = "-- Terms: --"
    ctxHeader   = "-- Contexts: --"

    ts = intersperse " " $ fmap (renderStyle fileLineStyle . ppr) (getTBinds' lib)
    cs = intersperse " " $ fmap (renderStyle fileLineStyle . ppr) (getCBinds' lib)

-- Cost-equivalent contexts: --------------------------------------------------

ctxEqsToFile :: CtxKind -> BaseLib -> [String]
ctxEqsToFile k lib = [""] ++ subHeader ++ ctxEqs
  where
    eqsHeader = "-- " ++ ctxKindToDescrip' k ++ ": --" 
    subHeader = [fileLineSep, eqsHeader, fileLineSep]
    ctxEqs    = (intersperse " " 
                 . fmap (renderStyle fileLineStyle . ppr) 
                 . (ctxKindToProj k) 
                 . ctxEqLib) lib 

-- Scripts: -------------------------------------------------------------------

scriptToFile :: TransEnv -> [String]
scriptToFile transEnv = [""] ++ subHeader ++ cmds
 where 
  scriptHeader = "-- Commands: --"
  subHeader    = [ fileLineSep, scriptHeader, fileLineSep ]
  cmds         = cmdHistToFileAsScript (cmdHist . getHist $ transEnv) 


-- /Strict/ line output
cmdHistToFileAsScript :: CmdHist -> [String]
cmdHistToFileAsScript  = fmap (renderStyle fileLineStyle . ppr . fst) . reverse

-------------------------------------------------------------------------------
-- Helpers: -- 
-------------------------------------------------------------------------------

-- Terminal helpers: ----------------------------------------------------------

-- Numbered bullet lists: --

terminalSingleNumberedList          ::  String -> [String] -> [String]
terminalSingleNumberedList title []  =  terminalAddTitleFrame title ["<empty>"]
terminalSingleNumberedList title xs  =  terminalAddTitleFrame title (singleNumber xs)

terminalMultiNumberedList           ::  String -> [[String]] -> [String]
terminalMultiNumberedList title []   =  terminalAddTitleFrame title ["<empty>"]
terminalMultiNumberedList title xss  =  terminalAddTitleFrame title (multiNumber xss)

terminalMultiNumberedList' :: String -> [[String]] -> [String]
terminalMultiNumberedList' title []  = terminalAddCentreHighlightedTitle title ["<empty>"]
terminalMultiNumberedList' title xss = terminalAddCentreHighlightedTitle title (multiNumber xss)

-- Highlighted numbered bullet list
terminalHighlightedMultiNumberedList :: String -> Int -> [[String]] -> [String]
terminalHighlightedMultiNumberedList title _  [] = terminalAddTitleFrame title ["<empty>"]
terminalHighlightedMultiNumberedList title idx xss  
 |  idx < 0          = terminalAddCentreHighlightedTitle          title     (multiNumber  xss)
 |  idx < length xss = terminalAddTitleFrameHighlightIdxHighlight title idx (multiNumber' xss)
 |  otherwise        = terminalAddCentreHighlightedTitle          title     (multiNumber  xss)

-- Bullet lists: -- 

terminalMultiDefaultBulletList :: String -> [[String]] -> [String]
terminalMultiDefaultBulletList title []  = terminalAddTitleFrame title ["<empty>"]
terminalMultiDefaultBulletList title xss = terminalAddTitleFrame title (multiBullet defaultBullet xss)
   
terminalMultiSpaceBulletList :: String -> [[String]] -> [String]
terminalMultiSpaceBulletList title []  = 
  terminalAddCentreHighlightedTitle title ["<empty>"]
terminalMultiSpaceBulletList title xss = 
  terminalAddCentreHighlightedTitle title (multiBullet' xss)


-- Add centred title with highlighting.
terminalAddCentreHighlightedTitle :: String -> [String] -> [String]
terminalAddCentreHighlightedTitle title xs = 
  ("\ESC[30m\ESC[3m\ESC[48;5;252m" ++ whitePadTo terminalLineWidth 
  (terminalCentreText title)  ++ "\ESC[m") : " " : xs


-- Add title and frame
terminalAddTitleFrame  ::  String -> [String] -> [String]
terminalAddTitleFrame title xs  
 =  line               -- top
     : take n (y : ys) -- title
     ++ line           -- sep
     : drop n (y : ys) -- content
     ++ [line]         -- bottom
    where 
         n        = length title'
         (y : ys) = fmap (\x -> "| " ++ x ++ " |") $ deggar (title' ++ xs)
         line     = '+' : replicate (length y - 2) '-' ++ "+"
         title'   = squashString terminalInnerFramedWidth title


terminalAddTitleFrameHighlightIdxHighlight :: String -> Int -> [[String]] -> [String]
terminalAddTitleFrameHighlightIdxHighlight title idx xss = concat (title' : rest)
  where
    title' = ["\ESC[30m\ESC[3m\ESC[48;5;252m" ++ whitePadTo terminalLineWidth 
               (terminalCentreText title)  ++ "\ESC[m", " "]
    rest = l ++ r' : others
    (l, r : others) = splitAt idx xss 
    r' =  fmap (\s -> "\ESC[30m\ESC[3m\ESC[48;5;252m" 
                 ++ whitePadTo terminalLineWidth s
                 ++ "\ESC[m") r



-- Add title, frame and highlighting
terminalAddTitleFrameHighlight ::  String -> Int -> [[String]] -> [String]
terminalAddTitleFrameHighlight title idx xss 
 =  line                  -- top 
     : title''            -- title
     ++ line              -- sep 
     : concat l           -- non-highlighted content
     ++ fmap highlight r  -- highlighted content
     ++ concat rs         -- non-highlighted content
     ++ [line]            -- bottom

    where 
         line             = '+' : replicate (n - 2) '-' ++ "+"
         highlight        = (\s -> "\ESC[30m\ESC[48;5;252m" ++ s ++ "\ESC[m")
         
         (l, r : rs)      = splitAt idx xss'
         -- I don't like this but deggar has the wrong type
         (title'' : xss')  = fmap (fmap (\x -> "| " ++ x ++ " |") 
                                  . fmap (whitePadTo maxLen)) (title' : xss)
         n                = length (head title'')
         maxLen           = maximum $ fmap length title' ++ concatMap (fmap length) xss
         title'           = squashString terminalInnerFramedWidth title

-- Add number to single line 
singleNumber    ::  [String] -> [String]
singleNumber xs  =  fmap (\(x, i) -> 
                    
                    let si  = show (i :: Int)
                        l   = length si 
                        pad = whitePad (max - l + 1)
                    in '(' : si ++ ')' : pad ++ x) 
                   
                    (zip xs [0..])
                    where max = length $ show $ length xs - 1

-- Add number/spacing to multiple lines and concatenate
multiNumber     ::  [[String]] -> [String]
multiNumber xss  =  concatMap (\(xs, i) -> 
                    
                    let si      = show (i :: Int)
                        l       = length si 
                        pad     = whitePad (max - l + 1)
                        padRest = whitePad (max + 3)
                    in case xs of 
                        []       -> []
                        (y : ys) -> ('(' : si ++ ')' : pad ++ y) 
                                     : fmap (padRest ++) ys) 
                   
                    (zip xss [0..])
                    where max = length $ show $ length xss - 1

-- Add number/spacing to multiple lines and concatenate
multiNumber'     ::  [[String]] -> [[String]]
multiNumber' xss  =  fmap (\(xs, i) -> 
                    
                     let si      = show (i :: Int)
                         l       = length si 
                         pad     = whitePad (max - l + 1)
                         padRest = whitePad (max + 3)
                     in case xs of 
                         []       -> []
                         (y : ys) -> ('(' : si ++ ')' : pad ++ y) 
                                      : fmap (padRest ++) ys) 
                    
                     (zip xss [0..])
                     where max = length $ show $ length xss - 1                  

-- Add bullet to single line 
singleBullet   ::  Char -> [String] -> [String]
singleBullet c  =  fmap (\s -> c : ' ' : s)

-- Add bullet/spacing to multiple lines and concatenate
multiBullet   ::  Char -> [[String]] -> [String]
multiBullet c  =  concatMap (\ss -> case ss of 
                   []       -> []
                   (x : xs) -> (c : ' ' : x) : fmap (whitePad 2 ++) xs)

-- Add a single space bullet
multiBullet' :: [[String]] -> [String]
multiBullet'  = concatMap (\ss -> case ss of 
                   []       -> []
                   (x : xs) -> (' ' : x) : fmap (whitePad 1 ++) xs)

-- Non-script line output for commands/parameters: --

-- Raw parameters
pprRawParamNS                              ::  RawParam -> Doc 
pprRawParamNS (RawSrcName s)                =  char '\'' <> text s
pprRawParamNS (RawSrcCode s)                =  char '$' <+> text (stripSpace s) <+> char '$'
pprRawParamNS (RawCmdName s)                =  text s 
pprRawParamNS (RawFile    s)                =  text s 
pprRawParamNS (RawNumber  n)                =  int n
pprRawParamNS (RawProp p1 pr p2)            =  pprRawParamsNS [p1, pr, p2] 

pprRawParamsNS                             ::  [RawParam] -> Doc 
pprRawParamsNS []                           =  empty
pprRawParamsNS (rp : rps)                   =  fsep [pprRawParamNS rp, pprRawParamsNS rps]

-- Raw commands with no location information on their parameters
pprRawCmdNoLocNS                           ::  RawCmd -> Doc 
pprRawCmdNoLocNS (RawNavCmd        s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]
pprRawCmdNoLocNS (RawShellCmd      s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]       
pprRawCmdNoLocNS (RawTransEnvCmd   s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]
pprRawCmdNoLocNS (RawBaseLibCmd    s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]
pprRawCmdNoLocNS (RawScriptCmd     s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]
pprRawCmdNoLocNS (RawKureCmd       s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]
pprRawCmdNoLocNS (RawStateCmd      s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]
pprRawCmdNoLocNS (RawAssumptionCmd s lrps)  =  fsep [text s, pprRawParamsNS (fmap par lrps)]

-- Located raw parameters
pprLocRawParamNS                           ::  LocatedRawParam -> Doc
pprLocRawParamNS (LocatedRawParam par pos)  =  hsep [ppr pos, pprRawParamNS par]

pprLocRawParamsNS                          ::  [LocatedRawParam] -> Doc 
pprLocRawParamsNS []                        =  empty
pprLocRawParamsNS (lrp : lrps)              =  pprLocRawParamNS lrp 
                                                $+$ pprLocRawParamsNS lrps

-- Parameters
pprParamNS                                 ::  Param -> Doc 
pprParamNS (CtxSrcName  s)                  =  text ('\'' : s)
pprParamNS (TermSrcName s)                  =  text ('\'' : s)
pprParamNS (CmdName     s)                  =  text s
pprParamNS (File        s)                  =  text s
pprParamNS (Number      n)                  =  int n 
pprParamNS (Rel         r)                  =  text (relToStr r)
pprParamNS (SrcCode     u)                  =  char '$' <+> ppr u    <+> char '$'
pprParamNS (TermSrcCode u)                  =  char '$' <+> ppr u    <+> char '$'
pprParamNS (CtxSrcCode  u)                  =  char '$' <+> ppr u    <+> char '$'
pprParamNS (PatSrcCode     upat)            =  char '$' <+> ppr upat <+> char '$'
pprParamNS (TermPatSrcCode upat)            =  char '$' <+> ppr upat <+> char '$'
pprParamNS (CtxPatSrcCode  upat)            =  char '$' <+> ppr upat <+> char '$'
pprParamNS (Prop p1 r p2)                   =  fsep [ pprParamNS p1
                                                    , pprParamNS (Rel r)
                                                    , pprParamNS p2 ]
pprParamNS (CtxKind k)                      =  text (show k)

-- Commands 
pprCmdNS                                   ::  Cmd -> Doc 
pprCmdNS (BaseLibCmd    s ps)               =  fsep (text s : fmap pprParamNS ps)
pprCmdNS (StateCmd      s ps)               =  fsep (text s : fmap pprParamNS ps)
pprCmdNS (ScriptCmd     s ps)               =  fsep (text s : fmap pprParamNS ps)
pprCmdNS (TransEnvCmd   s ps)               =  fsep (text s : fmap pprParamNS ps)
pprCmdNS (KureCmd       s ps)               =  fsep (text s : fmap pprParamNS ps)
pprCmdNS (ShellCmd      s ps)               =  fsep (text s : fmap pprParamNS ps)
pprCmdNS (AssumptionCmd s ps)               =  fsep (text s : fmap pprParamNS ps)
pprCmdNS (NavCmd dir)                       =  text (show dir)

-- Misc: ----------------------------------------------------------------------

-- Headers
colouredFancyHeader   ::  [String]
colouredFancyHeader    =  fmap (replicate 12 ' ' ++ ) unie

fileFancyHeader       ::  [String]
fileFancyHeader        =  fmap (centreText fileLineWidth) fancyHeader

fancyHeader  ::  [String]
fancyHeader            
 =  deggar [ "   _   _ _   _ _____ ____"
           , "  | | | | \\ | |_   _|  ___|      University of Nottingham"
           , "  | | | |  \\| | | | | |__           Improvement Engine"
           , "  | | | | . ` | | | |  __|"
           , "  | |_| | |\\  |_| |_| |___          Martin A.T. Handley"
           , "   \\___/\\_| \\_/\\___/\\____/   martin.handley@nottingham.ac.uk  "
           , ""
           , "   -.,-'~'-.,__,.-  ,__,.-'~'-.,__,.-' ~'-.,__,  -'~'-.,_"
           , ""
           ]

-- Auto generated message
autoGenMsg :: String
autoGenMsg  = "This file has been automatically generated by UNIE."


-- Testing UNIE logos: --------------------------------------------------------

u ::  [[String]]
u  =  replicate 4 [whiteSpace, whiteSpace, " ", " ", whiteSpace, whiteSpace, " "]
      ++ [replicate 6 whiteSpace ++ [" "]] 

n ::  [[String]]
n  =  reverse u

i ::  [[String]]
i  =   reverse $ 
       (replicate 2 whiteSpace ++ [" "]) : replicate 3 " " :
        replicate 3 (replicate 2 whiteSpace ++ [" "])

eInfo ::  [[String]]
eInfo  =  [ replicate 6 whiteSpace ++ 
         ["     \ESC[38;5;255mUniversity of Nottingham\ESC[m"]
      , [whiteSpace, whiteSpace, " " , " ", whiteSpace, whiteSpace, 
          "        \ESC[38;5;255mImprovement Engine\ESC[m"]
      , replicate 6 whiteSpace ++ [" "]
      , [whiteSpace, whiteSpace, " ", " ", " ", " ", 
          "        \ESC[38;5;255mMartin A.T. Handley\ESC[m"]
      , replicate 6 whiteSpace ++ 
         ["  \ESC[38;5;255mmartin.handley@nottingham.ac.uk\ESC[m"]
      ]

unie ::  [String]
unie  =  (fmap concat 
           . transpose 
           . concat
           . fmap transpose) [u, n, i, eInfo] 
          ++ [ "\ESC[38;5;255m_.-\"/______________////_\ESC[m"
             , "\ESC[38;5;255m`'-.\\--------------\\\\\\\\\"\ESC[m"
             ]
whiteSpace ::  String       
whiteSpace  =  "\ESC[48;5;255m \ESC[m" 

-------------------------------------------------------------------------------

fancyHeader'          ::  [String]
fancyHeader'           =  fancyHeader ++ [""]

fileFancyHeader'      ::  [String]
fileFancyHeader'       =  fileFancyHeader ++ [""]

-- Centre text in terminal
terminalCentreText    ::  String -> String 
terminalCentreText s   =  whitePad l ++ s
                          where l = (terminalLineWidth - length s) `div` 2

-- Centre text for a given width
centreText            ::  Int -> String -> String 
centreText l s         =  whitePad l' ++ s
                          where l' = (l - length s) `div` 2

-- Generate white padding
whitePad              ::  Int -> String
whitePad               =  flip replicate ' '

-- Add white padding on the right to a specific width
whitePadTo            ::  Int -> String -> String
whitePadTo i s         =  s ++ whitePad (i - length s)

-- Adding a character padding on the right to a specific width
padTo                 ::  Char -> Int -> String -> String
padTo c i s            =  s ++ replicate (i - length s) c

-- Chop a string to a specific width
chop                  ::  Int -> String -> String 
chop n s               |  length s < n = s 
                       |  otherwise    = take (n - 2) s ++ ".."

-- Unlines without the annoying newline for singletons
unlines'              ::  [String] -> String 
unlines' [x]           =  x
unlines' xs            =  unlines xs

-- Pretty names for CtxKinds
ctxKindToDescrip :: CtxKind -> String 
ctxKindToDescrip STD  = "Standard cost-equivalent contexts library"
ctxKindToDescrip VAL  = "Value cost-equivalent contexts library"    
ctxKindToDescrip EVAL = "Evaluation cost-equivalent contexts library"
ctxKindToDescrip APP  = "Applicative cost-equivalent contexts library"

-- Pretty names for CtxKinds
ctxKindToDescrip' :: CtxKind -> String 
ctxKindToDescrip' STD  = "Standard cost-equivalent contexts"
ctxKindToDescrip' VAL  = "Value cost-equivalent contexts"    
ctxKindToDescrip' EVAL = "Evaluation cost-equivalent contexts"
ctxKindToDescrip' APP  = "Applicative cost-equivalent contexts"

-- Replace newlines with spaces
newlinesToSpaces      ::  String -> String
newlinesToSpaces       =  unwords . fmap stripSpace . lines



-- Display library names used brick
displayLibNames :: String -> Map.Map String a -> InterM InterEnv ()
displayLibNames  = liftIO . display .* mapKeysToTerminal'