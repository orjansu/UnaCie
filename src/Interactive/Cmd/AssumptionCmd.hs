{-# LANGUAGE LambdaCase #-}

module AssumptionCmd
  ( assumptionCmds          -- All base lib. commands.matchers               
  , assumptionInterp        -- Interpreter for assumption commands.
  , matchers                -- Command line matchers for valid assumption commands.
  , refineRawAssumptionCmd  -- Refiner for raw assumption commands.
  ) where

import CmdAST        (Cmd(..), Param(..), RawCmd(..))
import CmdError      (CmdError(..), InternalError(..))
import InterState    (isTransState)
import InterUtils    (applyRTermCurrPathLog, outputCmdError)
import CmdParser     (cmdMatcher)
import ParamParser   (cmdNameMatcher, srcCodeMatcher)
import ParamRefine   (paramsRefine, relRefine', termSrcCodeRefine)
import Types         (Interp, Matcher, Refiner)
import Universes     (U(..))

import Data.Bifunctor (bimap)

{-
  Information:
  -----------------------------------------------------------------------------
  - A number of commands that constitute assumptions.
-}

-- Valid command names: -------------------------------------------------------

assumptionCmds :: [String]
assumptionCmds  = ["ass"]  -- Apply an assumption by rewriting a term.

-- Matchers for above commands: -----------------------------------------------

matchers :: [Matcher]
matchers  = [cmdMatcher "ass" RawAssumptionCmd [[cmdNameMatcher, srcCodeMatcher]]]

-- Refiner for above commands: ------------------------------------------------

refineRawAssumptionCmd :: Refiner
refineRawAssumptionCmd (RawAssumptionCmd "ass" ps) =
  bimap ParamErr (AssumptionCmd "ass") $ paramsRefine ps [[relRefine', termSrcCodeRefine]]
-- Anything else is invalid.
refineRawAssumptionCmd cmd@RawAssumptionCmd{} =  
  Left $ InternalErr $ UnexpectedCmd "refineRawAssumptionCmd" $ show cmd
refineRawAssumptionCmd _ = Left $ InternalErr $  WrongRefine "refineRawAssumptionCmd"                   

-- Interpreters for above commands, for all states: ---------------------------

assumptionInterp :: Interp 
assumptionInterp cmd _ st | isTransState st = go cmd
                          | otherwise = outputCmdError (StateErr st)

  where 
    go cmd@(AssumptionCmd "ass" ps) = case ps of
      [Rel r, TermSrcCode (UCtx t)] -> do 
        applyRTermCurrPathLog (cmd, Just r) (return t)
      _ -> outputCmdError . InternalErr  . UnexpectedParams 
            "assumptionInterp" $ fmap show ps
    
     -- Error cases.
    go cmd@TransEnvCmd{} = 
        outputCmdError $ InternalErr $ UnexpectedCmd  "assumptionInterp" $ show cmd
    go _ = outputCmdError $ InternalErr $ WrongInter "assumptionInterp"