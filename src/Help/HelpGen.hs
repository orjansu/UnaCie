
module HelpGen (gen) where

import Utils           (deggar)

import Control.Monad    (foldM)
import Data.Char        (toLower, toUpper)
import Data.List.Split  (splitOn) 
import Data.List.Utils  (replace)
import System.Directory (listDirectory)

{-
  Information:
  -----------------------------------------------------------------------------
  - Generates the 'help' file for UNIE.
-}

-- Filepath of man entries.
helpDir :: String 
helpDir  = "./Help Sections/"

-- Header stub.
header :: String
header = "\n-- This file has been automatically generated using HelpGen.hs.\
           \\n\nmodule Help (lookupHelp) where\
           \\n\nimport InterPrintUtils (terminalAddCentreHighlightedTitle)"

-- Top-level function for generating the man file.
gen :: IO () 
gen = do
         -- (1) Get all files in man folder, removing Template.
         fps <- listDirectory helpDir 

         -- (2) Read each file.
         lookups <- foldM (\acc fp -> do 
                      man <- readFile (helpDir ++ fp)
                      return ((fp, man) : acc)) [] fps

         -- (3) Output to single file
         let funs   = fmap (escape . genHelpEntry) lookups
         let helpFun = genHelpFun fps
  
         writeFile "./src/Help/Help.hs" $ unlines (header : helpFun : funs)
         putStrLn "Help file generated."

-- Generate a single help entry.
genHelpEntry :: (String, String) -> String 
genHelpEntry (help, info) = case lines info of 
  []         -> ""
  [info]     -> let ty  = help' ++ " :: [String]"
                    def = help' ++ "  = " ++ show [info]
                in unlines [ty, def]
  inf : infs -> let ty      = help' ++ " :: [String]"
                    fstDef  = help' ++ "  = terminalAddCentreHighlightedTitle "
                               ++ '\"' : help'' ++ "\""
                    restDef = fstLine inf : fmap nxtLine infs ++ [lstLine]
                in unlines $ [ty, fstDef] ++ restDef
 where 
  help'      = editHelpName help
  help''     = toUpper (head help) : tail help
  fstLine "" = "  [ " ++ show "    "
  fstLine s  = "  [ " ++ show s
  nxtLine "" = "  , " ++ show "    "
  nxtLine s  = "  , " ++ show s
  lstLine    = "  ]"

-- Generate the man lookup function.
genHelpFun :: [String] -> String 
genHelpFun fs = unlines $ [ty, def]
  where 
    ty      = "lookupHelp :: String -> Maybe [String]"
    def     = unlines $ "lookupHelp s = case s of" : lookups ++ [invalid]
    lookups = fmap (\(i, f) -> "  " ++ (fs' !! i) ++ " -> Just " ++ editHelpName f) 
               (zip [0..] fs)
    invalid = "  _ -> Nothing"
    fs'     = deggar $ fmap show fs

-- We have to manually add the backslash to escape codes
-- otherwise Haskell escapes it when it reads the file?
escape :: String -> String 
escape  = replace "ESC[" "\\ESC["  

-- Edit command names so they don't clash with Haskell keywords.
editHelpName :: String -> String 
editHelpName name = case chars of 
  [name]     -> lowerFirst name ++ "Help"
  (ns : nss) -> (lowerFirst ns) ++ concatMap upperFirst nss ++ "Help"
  []         -> ""
  where
    chars = splitOn "-" name
    upperFirst (c : cs) = toUpper c : cs
    upperFirst []       = []
    lowerFirst (c : cs) = toLower c : cs 
    lowerFirst []       = []