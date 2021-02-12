{-# LANGUAGE MultiWayIf #-}

module ShellCmd
 ( matchers           -- Command line matchers for valid shell commands.
 , refineRawShellCmd  -- Refiner for raw shell commands.
 , shellCmds          -- All shell commands.
 , shellInterp        -- Interpreter for shell commands.
 ) where

import CmdAST      (Cmd(..), Param(..), RawCmd(..))
import CmdError    (CmdError(..), InternalError(..))
import Display     (display)
import CmdParser   (cmdMatcher, cmdMatcherNoParams)
import Help        (lookupHelp)
import InterState  (isTransState)
import Man         (lookupMan)
import NavSettings (isHigh, updateHighPath)
import ParamParser (cmdNameMatcher)
import ParamRefine (anyCmdNameRefine, cmdNameRefine, paramsRefine)
import Types       (Interp, Matcher, Refiner)
import InterUtils
  ( InterM
  , getNavSettings
  , getPath
  , highlightNav
  , interPutError
  , interPutInfo
  , interPutWarning
  , modifyTransEnv
  , outputCmdError
  , putNavSettings
  , unHighlightNav
  )

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor         (bimap)
import System.Exit            (exitSuccess)
import System.Random          (randomRIO)

{-
  Information:
  -----------------------------------------------------------------------------
  - A number of commands for updating UNIE's shell settings/displaying
    help information.
-}

-- Valid command names: -------------------------------------------------------

shellCmds :: [String]
shellCmds  = [ {--"help",--} "highlight", "man", "quit", "zoom-in", "zoom-out" ]

-- Matchers for above commands: -----------------------------------------------

matchers :: [Matcher]
matchers  =
  [ cmdMatcherNoParams "quit" RawShellCmd                  -- Quit the system.
  , cmdMatcherNoParams "zoom-in"  RawShellCmd              -- Zoom into a sub-term.
  , cmdMatcherNoParams "zoom-out" RawShellCmd              -- Zoom out to expose full term.
  , cmdMatcher "help" RawShellCmd [[], [cmdNameMatcher]]   -- Show a help menu.
  , cmdMatcher "highlight" RawShellCmd [[cmdNameMatcher]]  -- Turn focus highlighting on/off.
  , cmdMatcher "man" RawShellCmd [[cmdNameMatcher]]        -- Show a man entry.
  ]


-- Refiner for above commands: ------------------------------------------------

refineRawShellCmd :: Refiner

-- No parameters for quit, zoom-in, zoom-out and help
refineRawShellCmd (RawShellCmd s [])
  | s `elem` ["quit", "zoom-in", "zoom-out", "help"] = Right (ShellCmd s [])
  | otherwise = Left $ InternalErr (WrongRefine "refineRawShellCmd")

-- help takes no parameter or a command name.
refineRawShellCmd (RawShellCmd "help" ps) =
  bimap ParamErr (ShellCmd "help") $ paramsRefine ps [[], [anyCmdNameRefine]]

-- highlight takes a command name
refineRawShellCmd (RawShellCmd "highlight" ps) =
  bimap ParamErr (ShellCmd "highlight") $ paramsRefine ps [[cmdNameRefine ["ON", "OFF"]]]

-- man takes a command name only.
refineRawShellCmd (RawShellCmd "man" ps) =
  bimap ParamErr (ShellCmd "man") $ paramsRefine ps [[anyCmdNameRefine]]

-- Anything else is invalid.
refineRawShellCmd _ = Left $ InternalErr (WrongRefine "refineRawShellCmd")


-- Interpreters for above commands, for all states: ---------------------------

shellInterp :: Interp

-- Quitting the system.
shellInterp (ShellCmd "quit" []) _ _ =
  liftIO (genGoodbyeMsg >>= putStrLn >> exitSuccess)
shellInterp (ShellCmd "quit" ps) _ _ = shellInterpUnexpectedParams ps

-- Display help menus.
shellInterp (ShellCmd "help" []) _ _ = case lookupHelp "introduction" of
  Just help -> liftIO $ display help
  Nothing   -> interPutError "no help file."
shellInterp (ShellCmd "help" ps) _ _ = case ps of
  [CmdName ns] -> case lookupHelp ns of
    Just help -> liftIO $ display help
    Nothing   -> interPutError $ "no help file for '" ++ ns ++ "'."
  _ -> shellInterpUnexpectedParams ps

-- Displaying man entries.
shellInterp (ShellCmd "man" ps) _ _ = case ps of
  [CmdName ns] -> case Man.lookupMan ns of
    Nothing   -> interPutError $ "no man entry for '" ++ ns ++ "'."
    Just info -> liftIO $ display info
  _ -> shellInterpUnexpectedParams ps

-- Highlighting the current focus.
shellInterp (ShellCmd "highlight" ps) _ st = case ps of
  [CmdName "ON"]  -> highlight True
  [CmdName "OFF"] -> highlight False
  _ -> shellInterpUnexpectedParams ps
  where highlight b
         | isTransState st = modifyTransEnv $ \transEnv ->
           let ns = getNavSettings transEnv
           in if | isHigh ns && b -> do
                    interPutWarning "focus highlighting already on."
                    return transEnv
                 | b -> do
                    interPutInfo "focus highlighting on."
                    return (highlightNav (getPath transEnv) transEnv)
                 | isHigh ns -> do
                    interPutInfo "focus highlighting off."
                    return (unHighlightNav transEnv)
                 | otherwise -> do
                    interPutWarning "focus highlighting already off."
                    return transEnv
         | otherwise = outputCmdError (StateErr st)

-- Zooming in.
shellInterp (ShellCmd "zoom-in" []) _ st
  | isTransState st = modifyTransEnv $ \transEnv ->
    let ns = getNavSettings transEnv
    in if isHigh ns
       then return (putNavSettings (updateHighPath (getPath transEnv) ns) transEnv)
       else interPutError "focus highlighting is off." >> return transEnv
  | otherwise = outputCmdError (StateErr st)
shellInterp (ShellCmd "zoom-in" ps) _ _ = shellInterpUnexpectedParams ps

-- Zooming out.
shellInterp (ShellCmd "zoom-out" []) _ st
 | isTransState st = modifyTransEnv $ \transEnv ->
   let ns = getNavSettings transEnv
   in if isHigh ns
      then return (putNavSettings (updateHighPath mempty ns) transEnv)
      else interPutError "focus highlighting is off." >> return transEnv
 | otherwise = outputCmdError (StateErr st)
shellInterp (ShellCmd "zoom-out" ps) _ _ = shellInterpUnexpectedParams ps

-- Error cases.
shellInterp cmd@ShellCmd{} _ _ =
  outputCmdError $ InternalErr $ UnexpectedCmd "shellInterp" $ show cmd
shellInterp _ _ _ = outputCmdError $ InternalErr $ WrongInter "shellInterp"



-- Helpers: -------------------------------------------------------------------

-- Error message for unexpected parameters.
shellInterpUnexpectedParams :: [Param] -> InterM s ()
shellInterpUnexpectedParams ps = outputCmdError
                                  . InternalErr
                                  . UnexpectedParams "shellInterp"
                                  $ fmap show ps

-- Goodbye messages that UNIE can display on exit.
genGoodbyeMsg :: IO String
genGoodbyeMsg  = (goodbyeMsgs !!) <$> randomRIO (0, length goodbyeMsgs - 1)

goodbyeMsgs :: [String]
goodbyeMsgs  = ["Thanks for using Unie!"]
