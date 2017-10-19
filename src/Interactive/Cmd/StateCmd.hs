
module StateCmd 
  ( matchers           -- Command line matchers for valid state commands.
  , refineRawStateCmd  -- Refiner for raw state commands.
  , stateCmds          -- All state commands.
  , stateInterp        -- Interpreter for state commands.
  ) where

import CmdAST        (Cmd(..), Param(..), RawCmd(..))
import CmdError      (CmdError(..), InternalError(..))
import CmdParser     (cmdMatcher, cmdMatcherNoParams)
import FileUtils     (getFileName)
import InterEnv      (InterEnv, initTransEnv)
import InterState    (InterState(..))
import ParamParser   ( cmdNameMatcher, fileMatcher, propMatcher 
                     , srcCodeMatcher, srcNameMatcher )
import ParamRefine   ( anyCmdNameRefine, ctxSrcCodeRefine, fileRefine 
                     , paramsRefine, propRefine, termSrcCodeRefine
                     , termSrcNameRefine )
import Relations     (Relation)
import Types         (Matcher, Refiner)
import Universes     (U(..))
import Utils         (deggar)
import InterUtils 
  ( InterM
  , exitActiveScript
  , getBaseLib
  , getInterEnv
  , fromScriptState
  , fromTransState
  , importScript
  , interPutError
  , interPutInfo
  , interPutStrLn
  , isActiveScript
  , loadScript
  , lookupTerm'
  , modifyInterEnv
  , nullTransEnv
  , outputCmdError
  , putInterEnv
  , putTransEnv
  , toScriptState
  , toTransState
  )

import Control.Monad.Extra    (ifM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor         (bimap)

{-
  < TO-DO>: - Refactor outputTransInfo. 

  Information:
  -----------------------------------------------------------------------------
  - A number of commands for updating UNIE's internal state;
  - Includes executing transformations and scripts.
-}

-- Valid command names: -------------------------------------------------------

stateCmds :: [String]
stateCmds  = [ "end-trans", "exit-script", "load-script", "run-script", "trans" ]

-- Matchers for above commands: -----------------------------------------------

matchers :: [Matcher]
matchers  = 
  [ cmdMatcherNoParams "end-trans" RawStateCmd                                          -- End the current program transformation.
  , cmdMatcherNoParams "exit-script"  RawStateCmd                                       -- Exit the active command script.
  , cmdMatcher "load-script"  RawStateCmd [[cmdNameMatcher]]                            -- Load a command script from UNIE's script library.
  , cmdMatcher "run-script"   RawStateCmd [[cmdNameMatcher], [fileMatcher]]             -- Run a command script from library or directly from file.
  , cmdMatcher "trans" RawStateCmd [[propMatcher], [srcNameMatcher], [srcCodeMatcher]]  -- Start a transformation.
  ]


-- Refiner for above commands: ------------------------------------------------

refineRawStateCmd :: Refiner

-- No parameters for exit-script and end-trans.
refineRawStateCmd (RawStateCmd s []) 
  | s `elem` ["end-trans", "exit-script"] = Right (StateCmd s [])
  | otherwise = Left $ InternalErr (WrongRefine "refineRawShellCmd")

-- trans takes a prop, term source name/source code.
refineRawStateCmd (RawStateCmd "trans" ps) = 
  bimap ParamErr (StateCmd "trans") $ paramsRefine ps 
   [[propRefine], [termSrcNameRefine], [termSrcCodeRefine], [ctxSrcCodeRefine]]

-- load-script takes any command name.
refineRawStateCmd (RawStateCmd "load-script"  ps) = 
  bimap ParamErr (StateCmd "load-script") $ paramsRefine ps [[anyCmdNameRefine]]

-- run-script takes a command name or a filepath.
refineRawStateCmd (RawStateCmd "run-script"   ps) =  
  bimap ParamErr (StateCmd "run-script") $ paramsRefine ps 
   [[anyCmdNameRefine], [fileRefine]]

-- Anything else is invalid.
refineRawStateCmd cmd@RawStateCmd{} =  
  Left $ InternalErr $ UnexpectedCmd "refineRawStateCmd" $ show cmd
refineRawStateCmd _  = Left $ InternalErr $ WrongRefine "refineRawStateCmd"


-- Interpreters for above commands, for all interpreter states: ---------------

stateInterp :: Cmd
               -> Maybe Relation
               -> InterState
               -> [Matcher]
               -> InterM InterEnv ()

stateInterp cmd mrel st matchers = case cmd of

 -- Commands handled uniformly in all states: ---------------------------------
 
  -- Load a script from the library.
  StateCmd "load-script" ps -> case ps of
    [CmdName s] -> loadScript_ s
    _           -> stateInterpUnexpectedParams ps 

  -- Run a script, either from the library or from file.
  -- ** Note: scripts can be run even when other scripts are running. **
  StateCmd "run-script" ps -> case ps of
    [CmdName s] -> loadScript_ s
    [File fp]   -> maybe (interPutError "invalid filepath.")
                         (\s -> importScript fp s matchers >> loadScript_ s)
                   =<< liftIO (getFileName fp)
    _ -> stateInterpUnexpectedParams ps 

 -- All other commmands are state dependent: ----------------------------------

  _ -> case st of
         INITIAL      -> goInit        cmd
         TRANS        -> goTrans       cmd
         SCRIPT       -> goScript      cmd
         TRANS_SCRIPT -> goTransScript cmd

 where

  -----------------------------------------------------------------------------
  -- INITIAL STATE: --
  -----------------------------------------------------------------------------

  -- Transform a prop/source code/source name.
  goInit (StateCmd "trans" ps) = interTransCmd ps
    
  -- End a transformation: not valid in INITIAL state.
  goInit (StateCmd "end-trans" _) = outputCmdError $ StateErr st

  -- Exit a script: not valid in INITIAL state.
  goInit (StateCmd "exit-script" _) = outputCmdError $ StateErr st

  -- Error cases for INITIAL state.
  goInit StateCmd{} = 
    outputCmdError $ InternalErr $ UnexpectedCmd "stateInterp" $ show cmd
  goInit _ = outputCmdError $ InternalErr $ WrongInter "stateInterp"

  -----------------------------------------------------------------------------
  -- TRANS STATE: --
  ----------------------------------------------------------------------------- 

  -- End a transformation.
  goTrans (StateCmd "end-trans" ps) = case ps of 
    [] -> endTrans
    _  -> stateInterpUnexpectedParams ps

  -- Exit a script: not valid in TRANS state.
  goTrans (StateCmd "exit-script" _) = outputCmdError $ StateErr st

  -- Transform a prop/source code/source name: not valid in TRANS state.
  goTrans (StateCmd "trans" _) = outputCmdError $ StateErr st

  -- Error cases for TRANS state.
  goTrans StateCmd{} = 
    outputCmdError $ InternalErr $ UnexpectedCmd "stateInterp" $ show cmd
  goTrans _ = outputCmdError $ InternalErr $ WrongInter "stateInterp"

  -----------------------------------------------------------------------------
  -- SCRIPT STATE: --
  -----------------------------------------------------------------------------

  -- Transform a prop/source code/source name.
  goScript (StateCmd "trans" ps) = interTransCmd ps  

  -- Exit current script, but load next if others are still active.
  goScript (StateCmd "exit-script" ps) = case ps of
    [] -> exitScript 
    _  -> stateInterpUnexpectedParams ps

  -- End a transformation: not valid in SCRIPT state.
  goScript (StateCmd "end-trans" _) = outputCmdError $ StateErr st

  -- Error cases for SCRIPT state.
  goScript StateCmd{} = 
    outputCmdError $ InternalErr $ UnexpectedCmd "stateInterp" $ show cmd
  goScript _  = outputCmdError $ InternalErr $ WrongInter "stateInterp"

  -----------------------------------------------------------------------------
  -- TRANS_SCRIPT STATE: --
  -----------------------------------------------------------------------------

  -- Exit current script, but load next if others are still active.
  goTransScript (StateCmd "exit-script" ps) = case ps of
    [] -> exitScript 
    _  -> stateInterpUnexpectedParams ps

  -- End a transformation.
  goTransScript (StateCmd "end-trans" ps) = case ps of 
    [] -> endTrans
    _  -> stateInterpUnexpectedParams ps

  -- Transform a prop/source code/source name: not valid in TRANS_SCRIPT state.
  goTransScript (StateCmd "trans" _) = outputCmdError $ StateErr st

  -- Error cases for TRANS_SCRIPT state.
  goTransScript StateCmd{} = 
    outputCmdError $ InternalErr $ UnexpectedCmd "stateInterp" $ show cmd
  goTransScript _ = outputCmdError $ InternalErr $ WrongInter "stateInterp"

  -----------------------------------------------------------------------------
  -- HELPERS: --
  -----------------------------------------------------------------------------

  -- Transform a prop/source code/source name: --------------------------------

  interTransCmd :: [Param] -> InterM InterEnv ()

  -- SrcCode.
  interTransCmd [TermSrcCode (UCtx t)] = do 
    baseLib <- getInterEnv getBaseLib
    startTrans $ initTransEnv baseLib t Nothing Nothing (cmd, mrel)

  interTransCmd [CtxSrcCode (UCtx ctx)] = do 
    baseLib <- getInterEnv getBaseLib
    startTrans $ initTransEnv baseLib ctx Nothing Nothing (cmd, mrel)

  -- SrcName.
  interTransCmd [TermSrcName s] = do 
    baseLib <- getInterEnv getBaseLib
    case lookupTerm' s baseLib of
      Just t  -> startTrans $ initTransEnv baseLib t Nothing Nothing (cmd, mrel)
      Nothing -> interPutError $ notFoundError s

  -- Prop = SrcName REL SrcName.
  interTransCmd [Prop (TermSrcName s1) rel (TermSrcName s2)] = do
    baseLib <- getInterEnv getBaseLib
    case sequence [lookupTerm' s1 baseLib, lookupTerm' s2 baseLib] of
      Just [t1, t2] -> do 
        outputTransInfo (rel, t2)
        startTrans $ initTransEnv baseLib t1 (Just rel) (Just t2) (cmd, mrel)
      _ -> interPutError $ "'" ++ s1 ++ "' and/or '"
                               ++ s2 ++ "' not found in library."

  -- Prop = ScrCode REL SrcCode.
  interTransCmd [Prop (TermSrcCode (UCtx t1)) rel (TermSrcCode (UCtx t2))] = do 
    baseLib <- getInterEnv getBaseLib
    outputTransInfo (rel, t2)
    startTrans $ initTransEnv baseLib t1 (Just rel) (Just t2) (cmd, mrel)

  -- Prop = SrcName REL SrcCode.
  interTransCmd [Prop (TermSrcName s) rel (TermSrcCode (UCtx t2))] = do 
    baseLib <- getInterEnv getBaseLib
    case lookupTerm' s baseLib of
      Just t1 -> do 
        outputTransInfo (rel, t2)
        startTrans $ initTransEnv baseLib t1 (Just rel) (Just t2) (cmd, mrel)
      Nothing -> interPutError $ notFoundError s

  -- Prop = SrcCode REL SrcName.
  interTransCmd [Prop (TermSrcCode (UCtx t1)) rel (TermSrcName s)] = do 
    baseLib <- getInterEnv getBaseLib
    case lookupTerm' s baseLib of
      Just t2 -> do 
        outputTransInfo (rel, t2)
        startTrans $ initTransEnv baseLib t1 (Just rel) (Just t2) (cmd, mrel)
      Nothing -> interPutError $ notFoundError s

  -- Error case.
  interTransCmd ps = stateInterpUnexpectedParams ps

  -----------------------------------------------------------------------------

  -- Load a script from the /script library.
  loadScript_ s = maybe (interPutError $ notFoundError s)
                        (\env -> putInterEnv env >> toScriptState)
                 =<< getInterEnv (loadScript s)

  -- Start a transformation.
  startTrans transEnv = do 
    interPutInfo "transformation started."
    modifyInterEnv (putTransEnv transEnv) >> toTransState

  -- End a transformation.
  endTrans = interPutInfo "transformation ended."
               >> modifyInterEnv nullTransEnv
               >> fromTransState

  -- Exit a script.
  exitScript = do modifyInterEnv exitActiveScript
                  interPutInfo "script exited."
                  ifM (getInterEnv isActiveScript)
                      (return ())
                      fromScriptState

  -- Gen. not found error message.
  notFoundError s = '\'' : s ++ "' not found in library."

  -- Print transformation's global relation/goal.
  outputTransInfo (rel, goal) = do 
    
    -- Global relation.
    interPutStrLn $ "\n\ESC[38;5;15m Global relation set: " 
        ++ show rel ++ " \ESC[m"

    -- Transformation goal, check if multi-line.
    if '\n' `elem` sgoal 
    then do 
      -- One line spacing before.
      interPutStrLn $ "\n\ESC[38;5;15m Transformation goal set:\ESC[m"
      mapM_ interPutStrLn $ (fmap (\s -> "\ESC[38;5;15m    " 
        ++ s ++ " \ESC[m") . deggar . lines $ sgoal)
    -- No spacing before.
    else interPutStrLn $ "\ESC[38;5;15m Transformation goal set: " 
        ++ sgoal ++ "\ESC[m"

    interPutStrLn "" -- Always one line spacing after.

    where sgoal = show goal 


-- Helpers: -------------------------------------------------------------------

-- Error message for unexpected parameters.
stateInterpUnexpectedParams :: [Param] -> InterM s ()
stateInterpUnexpectedParams ps = outputCmdError 
                                  . InternalErr 
                                  . UnexpectedParams "stateInterp" 
                                  $ fmap show ps