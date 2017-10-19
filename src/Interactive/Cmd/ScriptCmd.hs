{-# LANGUAGE LambdaCase #-}

module ScriptCmd
  ( matchers             -- Command line matchers for valid script commands.
  , refineRawScriptCmd   -- Refiner for raw script commands.
  , scriptCmds           -- All script commands.
  , scriptInterp         -- Interpreter for script commands.
  ) where

import CmdAST          (Cmd(..), Param(..), RawCmd(..))
import CmdError        (CmdError(..), InternalError(..))
import CmdParser       (cmdMatcher, cmdMatcherNoParams)
import Display         (display)
import InterEnv        (InterEnv)
import InterPrintUtils ( activeScriptToTerminal, displayLibNames
                       , scriptToFile, scriptToTerminal )
import InterState      (InterState(..))
import ParamParser     (cmdNameMatcher, fileMatcher)
import ParamRefine     (anyCmdNameRefine, fileRefine, paramsRefine)
import Relations       (Relation)
import Types           (Matcher, Refiner)
import InterUtils 
  ( InterM
  , getActiveScripts
  , getInterEnv
  , getScript
  , getScripts
  , importScript
  , interPutInfo
  , modifyInterEnv
  , nextActiveScriptCmd
  , outputCmdError
  , outputLibNotFoundError'
  , prevActiveScriptCmd
  , putInterEnv
  , reloadActiveScript
  , scriptEnvCritError
  , transEnvExport
  )

import Data.Bifunctor         (bimap)
import Control.Monad.IO.Class (liftIO)

{-
  Information:
  -----------------------------------------------------------------------------
  - A number of commands for interactive with improvement command scripts;
  - Including importing/exporting/loading/displaying.
-}

-- Valid command names: -------------------------------------------------------

scriptCmds ::  [String]
scriptCmds  =  [ "export-script"        -- Export the command history to file as a command script.
               , "import-script"        -- Import a command script from file.
               , "next-cmd"             -- Skip to the next command in an active script.
               , "prev-cmd"             -- Skip to the previous command in an active script.
               , "reset-active-script"  -- Reset the active script by skipping to the first command.
               , "show-active-script"   -- Display the sequence of commands that make up the active script.
               , "show-script"          -- Display the sequence of commands that make up a given script.
               , "show-scripts" ]       -- Display the names of all commands in the library.

-- Matchers for above commands: -----------------------------------------------

matchers :: [Matcher]
matchers  = 
  [ cmdMatcher "export-script" RawScriptCmd [[fileMatcher]]
  , cmdMatcher "import-script" RawScriptCmd [[cmdNameMatcher, fileMatcher]]
  , cmdMatcher "show-script"   RawScriptCmd [[cmdNameMatcher]]
  , cmdMatcherNoParams "next-cmd" RawScriptCmd 
  , cmdMatcherNoParams "prev-cmd" RawScriptCmd 
  , cmdMatcherNoParams "reset-active-script" RawScriptCmd 
  , cmdMatcherNoParams "show-active-script"  RawScriptCmd 
  , cmdMatcherNoParams "show-scripts" RawScriptCmd
  ]

-- Refiner for above commands: ------------------------------------------------

refineRawScriptCmd :: Refiner

-- Export just needs a filepath.
refineRawScriptCmd (RawScriptCmd "export-script" ps) = 
  bimap ParamErr (ScriptCmd "export-script") $ paramsRefine ps [[fileRefine]]

-- Import script needs a name to save the script under, and a filepath to import.
refineRawScriptCmd (RawScriptCmd "import-script" ps) = 
  bimap ParamErr (ScriptCmd "import-script") $ paramsRefine ps 
   [[anyCmdNameRefine, fileRefine]]

-- Needs name of script to show.
refineRawScriptCmd (RawScriptCmd "show-script" ps) = 
  bimap ParamErr (ScriptCmd "show-script") $ paramsRefine ps [[anyCmdNameRefine]]

-- All other commands don't require parameters.
refineRawScriptCmd (RawScriptCmd "show-scripts" ps) = 
  bimap ParamErr (ScriptCmd "show-scripts") $ paramsRefine ps [[]]
refineRawScriptCmd (RawScriptCmd "show-active-script"  ps) = 
  bimap ParamErr (ScriptCmd "show-active-script") $ paramsRefine ps [[]]
refineRawScriptCmd (RawScriptCmd "reset-active-script" ps) = 
  bimap ParamErr (ScriptCmd "reset-active-script") $ paramsRefine ps [[]]
refineRawScriptCmd (RawScriptCmd "next-cmd" ps) = 
  bimap ParamErr (ScriptCmd "next-cmd") $ paramsRefine ps [[]]
refineRawScriptCmd (RawScriptCmd "prev-cmd" ps) = 
  bimap ParamErr (ScriptCmd "prev-cmd") $ paramsRefine ps [[]]

-- Error cases.
refineRawScriptCmd cmd@RawScriptCmd{}  = 
    Left $ InternalErr $ UnexpectedCmd "refineRawScriptCmd" $ show cmd
refineRawScriptCmd _ = Left $ InternalErr $ WrongRefine "refineRawScriptCmd"

-- Interpreters for above commands, for all states: ---------------------------

-- Takes an extra parameters of mathcers for parsing script files.
scriptInterp :: Cmd
                -> Maybe Relation
                -> InterState
                -> [Matcher]
                -> InterM InterEnv ()
scriptInterp cmd _ st matchers = case cmd of

 -- Commands handled uniformly in all states: ---------------------------------
 
     ScriptCmd "import-script" ps -> case ps of
       [CmdName s,File fp] -> importScript fp s matchers
       _ -> scriptInterpUnexpectedParams ps

     ScriptCmd "show-scripts" ps -> case ps of
       [] -> displayLibNames "Script library" =<< getInterEnv getScripts
       _  -> scriptInterpUnexpectedParams ps

     ScriptCmd "show-script" ps -> case ps of
       [CmdName s] -> maybe (outputLibNotFoundError' s) 
                            (liftIO . display . scriptToTerminal s)
                      =<< getInterEnv (getScript s)
       _ -> scriptInterpUnexpectedParams ps

 -- State dependent commands: -------------------------------------------------
     
     _ -> case st of
            INITIAL      -> goInit        cmd
            TRANS        -> goTrans       cmd
            SCRIPT       -> goScript      cmd
            TRANS_SCRIPT -> goTransScript cmd

  where

    -- INITIAL STATE: ---------------------------------------------------------

    -- These commands are only available if a script is active.
    goInit (ScriptCmd "export-script"       _) = outputCmdError (StateErr st)
    goInit (ScriptCmd "show-active-script"  _) = outputCmdError (StateErr st)
    goInit (ScriptCmd "reset-active-script" _) = outputCmdError (StateErr st)
    goInit (ScriptCmd "next-cmd"            _) = outputCmdError (StateErr st)
    goInit (ScriptCmd "prev-cmd"            _) = outputCmdError (StateErr st)

    -- Error cases.
    goInit ScriptCmd{} = outputCmdError $ InternalErr $ UnexpectedCmd 
                           "scriptInterp" $ show cmd
    goInit _ = outputCmdError $ InternalErr $ WrongInter "scriptInterp"

    -- TRANS STATE: -----------------------------------------------------------

    goTrans (ScriptCmd "export-script" ps) = case ps of
      [File fp] -> transEnvExport fp scriptToFile
      _ -> scriptInterpUnexpectedParams ps

    -- Most script commands require a script state, so these are all state
    -- errors.
    goTrans (ScriptCmd "show-active-script"  _) = outputCmdError (StateErr st)
    goTrans (ScriptCmd "reset-active-script" _) = outputCmdError (StateErr st)
    goTrans (ScriptCmd "next-cmd"            _) = outputCmdError (StateErr st)
    goTrans (ScriptCmd "prev-cmd"            _) = outputCmdError (StateErr st)

    -- Error cases.
    goTrans ScriptCmd{} = outputCmdError $ InternalErr $ UnexpectedCmd 
                           "scriptInterp" $ show cmd
    goTrans _ = outputCmdError $ InternalErr $ WrongInter "scriptInterp"

    -- SCRIPT STATE: ----------------------------------------------------------

    -- See helpers below.
    goScript (ScriptCmd "show-active-script" ps)  = showActiveScript ps
    goScript (ScriptCmd "reset-active-script" ps) = resetActiveScript ps
    goScript (ScriptCmd "next-cmd" ps)            = nextCmd ps 
    goScript (ScriptCmd "prev-cmd" ps)            = prevCmd ps

    -- Can't export script unless in a transformation state.
    goScript (ScriptCmd "export-script" _) = outputCmdError (StateErr st)

    -- Error cases.
    goScript ScriptCmd{} = outputCmdError $ InternalErr $ UnexpectedCmd 
                            "scriptInterp" $ show cmd
    goScript _ = outputCmdError $ InternalErr $ WrongInter "scriptInterp"

    -- TRANS_SCRIPT STATE: ----------------------------------------------------
    
    -- See helpers below.
    goTransScript (ScriptCmd "show-active-script" ps)  = showActiveScript ps
    goTransScript (ScriptCmd "reset-active-script" ps) = resetActiveScript ps
    goTransScript (ScriptCmd "next-cmd" ps)            = nextCmd ps 
    goTransScript (ScriptCmd "prev-cmd" ps)            = prevCmd ps
    
    goTransScript (ScriptCmd "export-script" ps) = case ps of
      [File fp] -> transEnvExport fp scriptToFile
      _ -> scriptInterpUnexpectedParams ps
   
    -- Error cases.
    goTransScript ScriptCmd{} = outputCmdError $ InternalErr $ UnexpectedCmd 
                                 "scriptInterp" $ show cmd
    goTransScript _ = outputCmdError $ InternalErr $ WrongInter "scriptInterp"

    -- HELPERS: ---------------------------------------------------------------
     
    -- If we execute active script commands and we aren't in a script mode, 
    -- it's a critical system error (should never happen).

    showActiveScript :: [Param] -> InterM InterEnv ()
    showActiveScript ps = case ps of
      [] -> getInterEnv getActiveScripts >>= \case
              []      -> scriptEnvCritError
              (s : _) -> liftIO . display . activeScriptToTerminal $ s
      _  -> scriptInterpUnexpectedParams ps 

    resetActiveScript :: [Param] -> InterM InterEnv ()
    resetActiveScript ps = case ps of
      [] -> getInterEnv reloadActiveScript >>= \case
                Nothing  -> scriptEnvCritError
                Just env -> putInterEnv env >> interPutInfo "active script reset."
      _  -> scriptInterpUnexpectedParams ps

    -- Next/prev script command: --

    nextCmd :: [Param] -> InterM InterEnv ()
    nextCmd ps = case ps of
      [] -> modifyInterEnv nextActiveScriptCmd
      _  -> scriptInterpUnexpectedParams ps

    prevCmd :: [Param] -> InterM InterEnv ()
    prevCmd ps = case ps of
      [] -> modifyInterEnv prevActiveScriptCmd
      _  -> scriptInterpUnexpectedParams ps

-- Helpers: -------------------------------------------------------------------

-- Error message for unexpected parameters.
scriptInterpUnexpectedParams :: [Param] -> InterM s ()
scriptInterpUnexpectedParams ps = outputCmdError 
                                   . InternalErr 
                                   . UnexpectedParams "scriptInterp" 
                                   $ fmap show ps