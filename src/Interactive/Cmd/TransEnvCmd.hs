{-# LANGUAGE MultiWayIf #-}

module TransEnvCmd
  ( matchers               -- Command line matchers for valid trans env. commands.
  , refineRawTransEnvCmd   -- Refiner for raw trans env. commands.
  , transEnvCmds           -- All trans. env. commands.
  , transEnvInterp         -- Interpreter for trans. env. commands.
  ) where

import CmdAST          (Cmd(..), Param(..), RawCmd(..))
import CmdError        (CmdError(..), InternalError(..))
import CmdParser       (cmdMatcher, cmdMatcherNoParams)
import Display         (display)
import InterPrintUtils ( cmdHistToTerminal, goalToTerminal
                       , grelToTerminal, stateHistToFile
                       , stateHistToTerminal, transHistToFile
                       , transHistToTerminal )
import InterState      (isTransState)
import ParamParser     (cmdNameMatcher, fileMatcher, numberMatcher)
import ParamRefine     ( cmdNameRefine, fileRefine
                       , numberRefine, paramsRefine)
import TransHist       ( cmdHist, delHist, emptyTransHist
                       , maxHistIndex )
import Types           (Interp, Matcher, Refiner)
import InterUtils
  (InterM
  , getHist
  , getTransEnv
  , interPutError
  , interPutInfo
  , modHist
  , modifyTransEnv
  , outputCmdError
  , putHist
  , revertHist
  , transEnvExport
  )

import Data.Bifunctor (bimap)
import Control.Monad.IO.Class (liftIO)

{-
  Information:
  -----------------------------------------------------------------------------
  - A number of commands for updating UNIE's transformation environment.
-}

-- Valid command names: -------------------------------------------------------

transEnvCmds :: [String]
transEnvCmds  = [ "show-hist"    -- Display the program transformation's history.
                , "del-hist"     -- Delete the transformation's history up to an index.
                , "export-hist"  -- Export the transformation's history to file.
                , "back-hist"    -- Go back in the transformation's history.
                , "show-goal"    -- Show the transformation's goal.
                , "show-grel"    -- Show the transformation's global relation.
                ]

-- Matchers for above commands: -----------------------------------------------

matchers :: [Matcher]
matchers  = 
  [ cmdMatcher "show-hist"   RawTransEnvCmd [[], [cmdNameMatcher]]
  , cmdMatcher "del-hist"    RawTransEnvCmd [[], [numberMatcher]]
  , cmdMatcher "export-hist" RawTransEnvCmd [ [fileMatcher]
                                            , [cmdNameMatcher, fileMatcher] ]
  , cmdMatcherNoParams "back-hist" RawTransEnvCmd 
  , cmdMatcherNoParams "show-goal" RawTransEnvCmd 
  , cmdMatcherNoParams "show-grel" RawTransEnvCmd
  ]

-- Refiner for above commands: ------------------------------------------------

refineRawTransEnvCmd :: Refiner 

refineRawTransEnvCmd (RawTransEnvCmd "show-hist" ps) = 
  bimap ParamErr (TransEnvCmd "show-hist") $
   paramsRefine ps [[], [cmdNameRefine ["cmds", "state"]]]

refineRawTransEnvCmd (RawTransEnvCmd "del-hist" ps) = 
  bimap ParamErr (TransEnvCmd "del-hist") $
   paramsRefine ps [[], [numberRefine]]

refineRawTransEnvCmd (RawTransEnvCmd "export-hist" ps) = 
  bimap ParamErr (TransEnvCmd "export-hist") $
   paramsRefine ps [ [fileRefine]
                   , [ cmdNameRefine ["state"], fileRefine]
                   ]

-- No parameters.
refineRawTransEnvCmd (RawTransEnvCmd "back-hist" []) = Right (TransEnvCmd "back-hist" [])
refineRawTransEnvCmd (RawTransEnvCmd "show-goal" []) = Right (TransEnvCmd "show-goal" [])
refineRawTransEnvCmd (RawTransEnvCmd "show-grel" []) = Right (TransEnvCmd "show-grel" [])

-- Error cases.
refineRawTransEnvCmd cmd@RawTransEnvCmd{} = 
  Left $ InternalErr $ UnexpectedCmd "refineRawTransEnvCmd" $ show cmd
refineRawTransEnvCmd _ = Left $ InternalErr $ WrongRefine "refineRawTransEnvCmd"


-- Interpreters for above commands, for all states: ---------------------------

-- /All/ trans. env. commands are only available in a transformation state.
transEnvInterp :: Interp
transEnvInterp cmd _ st | isTransState st = go cmd
                        | otherwise = outputCmdError (StateErr st)
 where
  go (TransEnvCmd "show-hist" ps) = case ps of
  
   -- Show history laid out as a proof.
   [] -> getTransEnv $ liftIO . display . transHistToTerminal . getHist

   -- Show command history only.
   [CmdName "cmds"] -> getTransEnv $ liftIO . display . cmdHistToTerminal . cmdHist . getHist

   -- Show proof state history only.
   [CmdName "state"] -> getTransEnv $ liftIO . display . stateHistToTerminal . getHist

   -- Anything else is invalid.
   _ -> transEnvUnexpectedParams ps

  -- Delete transformation history.
  go (TransEnvCmd "del-hist" ps) = case ps of
   -- Delete all history.
   [] -> modifyTransEnv $ \transEnv ->
          if getHist transEnv == emptyTransHist
          then interPutError "history is empty." >> return transEnv
          else do 
            interPutInfo "history updated."
            return (putHist emptyTransHist transEnv)

   -- Delete up to a given index (inclusive).
   [Number i] -> modifyTransEnv $ \transEnv ->
     let h = getHist transEnv
     in if | h == emptyTransHist -> interPutError "history is empty." >> return transEnv
           | maxHistIndex h < i  -> interPutError "invalid index."    >> return transEnv
           | otherwise -> interPutInfo "history updated." >> return (modHist (delHist i) transEnv)

   -- Anything else is invalid.
   _ -> transEnvUnexpectedParams ps

  -- Export transformation history to file.
  go (TransEnvCmd "export-hist" ps) = case ps of
    [File fp] -> transEnvExport fp transHistToFile
    -- Can export just the states too.
    [CmdName "state", File fp] -> transEnvExport fp stateHistToFile
    _ -> transEnvUnexpectedParams ps
  
  -- Revert the history by going back to a previous proof state.
  go (TransEnvCmd "back-hist" []) = modifyTransEnv $ \transEnv ->
    if maxHistIndex (getHist transEnv) == 0
    then interPutError "no previous history." >> return transEnv
    else interPutInfo "history updated." >> return (revertHist transEnv)

  -- Show transformation's goal/global relation.
  go (TransEnvCmd "show-goal" []) = getTransEnv $ liftIO . display . goalToTerminal
  go (TransEnvCmd "show-grel" []) = getTransEnv $ liftIO . display . grelToTerminal

  -- Error cases.
  go cmd@TransEnvCmd{} =  
    outputCmdError $ InternalErr $ UnexpectedCmd  "transEnvInterp" $ show cmd
  go _ = outputCmdError $ InternalErr $ WrongInter "transEnvInterp"

-- Helpers: -------------------------------------------------------------------

-- Error message for unexpected parameters.
transEnvUnexpectedParams :: [Param] -> InterM s ()
transEnvUnexpectedParams ps = outputCmdError 
                               . InternalErr 
                               . UnexpectedParams "transEnvInterp" 
                               $ fmap show ps