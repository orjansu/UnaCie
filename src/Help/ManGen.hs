
module ManGen (gen) where 

import Utils (deggar)

import Control.Monad    (foldM)
import Data.Char        (toLower, toUpper)
import Data.List        ((\\))
import Data.List.Split  (splitOn) 
import Data.List.Utils  (replace)
import System.Directory (listDirectory)

{-
  Information:
  -----------------------------------------------------------------------------
  - Generates the 'man' file for UNIE.
-}

-- Filepath of man entries.
manDir :: String 
manDir  = "./Man/"

-- Header stub.
header :: String
header = "\n-- This file has been automatically generated using ManGen.hs.\
           \\n\nmodule Man (lookupMan) where\n"

-- Top-level function for generating the man file.
gen :: IO () 
gen = do
         -- (1) Get all files in man folder, removing Template.
         fps <- (\\ ["Template"]) <$> listDirectory manDir 

         -- (2) Read each file.
         lookups <- foldM (\acc fp -> do 
                      man <- readFile (manDir ++ fp)
                      return ((fp, man) : acc)) [] fps

         -- (3) Output to single file
         let funs   = fmap (escape . genManEntry) lookups
         let manFun = genManFun fps
  
         writeFile "./src/Help/Man.hs" $ unlines (header : manFun : funs)
         putStrLn "Man file generated."

-- Generate a single man entry.
genManEntry :: (String, String) -> String 
genManEntry (man, info) = case lines info of 
  []         -> ""
  [info]     -> let ty  = man' ++ " :: [String]"
                    def = man' ++ "  = " ++ show [info]
                in unlines [ty, def]
  inf : infs -> let ty      = man' ++ " :: [String]"
                    fstDef  = man' ++ "  ="
                    restDef = fstLine inf : fmap nxtLine infs ++ [lstLine]
                in unlines $ [ty, fstDef] ++ restDef
 where 
  man'       = editCmdName man
  fstLine "" = "  [ " ++ show "    "
  fstLine s  = "  [ " ++ show s
  nxtLine "" = "  , " ++ show "    "
  nxtLine s  = "  , " ++ show s
  lstLine    = "  ]"

-- Generate the man lookup function.
genManFun :: [String] -> String 
genManFun fs = unlines $ [ty, def]
  where 
    ty      = "lookupMan :: String -> Maybe [String]"
    def     = unlines $ "lookupMan s = case s of" : lookups ++ [invalid]
    lookups = fmap (\(i, f) -> "  " ++ (fs' !! i) ++ " -> Just " ++ editCmdName f) 
               (zip [0..] fs)
    invalid = "  _ -> Nothing"
    fs'     = deggar $ fmap show fs

-- We have to manually add the backslash to escape codes
-- otherwise Haskell escapes it when it reads the file?
escape :: String -> String 
escape  = replace "ESC[" "\\ESC["  

-- Edit command names so they don't clash with Haskell keywords.
editCmdName :: String -> String 
editCmdName name = case chars of 
  [name]     -> lowerFirst name ++ "Cmd"
  (ns : nss) -> (lowerFirst ns) ++ concatMap upperFirst nss ++ "Cmd"
  []         -> ""
  where
    chars = splitOn "-" name
    upperFirst (c : cs) = toUpper c : cs
    upperFirst []       = []
    lowerFirst (c : cs) = toLower c : cs 
    lowerFirst []       = []