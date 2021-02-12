
module NavCmd
  ( matchers         -- Command line matchers for valid nav. commands.
  , navCmds          -- All nav. commands.
  , navInterp        -- Interpreter for nav. commands.
  , refineRawNavCmd  -- Refiner for raw nav. commands.
  ) where

import CmdAST     (Cmd(..), RawCmd(..))
import CmdError   (CmdError(..), InternalError(..))
import CmdParser  (cmdMatcherNoParams)
import InterState (isTransState)
import InterUtils ( getPath, getTerm, modifyTransEnv
                  , outputCmdError, putPath, transExec
                  , update)
import Navigation (navigateUT, strToDir)
import Types      (Interp, Matcher, Refiner)

{-
  <TO-DO>: - Add higher-level navigation commands.

  Information:
  -----------------------------------------------------------------------------
  - A number of commands for navigating the abstract syntax tree of a term
    currently being transformed.
-}

-- Valid command names: -------------------------------------------------------

navCmds :: [String]
navCmds  = [ "top"    -- Return to the top of the AST.
           , "up"     -- Return to the current node's parent node.
           , "left"   -- Descend to the current node's left child node.
           , "right"  -- Descend to the current node's right child node.
           , "next"   -- Descend to the next alt/binding node.
           , "prev"   -- Return to the previous alt/binding node.
           , "rhs"    -- Go to the RHS of the current alt/binding node.
           ]

-- Matchers for above commands: -----------------------------------------------

-- All commands above take no parameters.
matchers :: [Matcher]
matchers  = fmap (flip cmdMatcherNoParams RawNavCmd) navCmds

-- Refiner for above commands: ------------------------------------------------

refineRawNavCmd :: Refiner
-- Just make sure the given command has no parameters and is a
-- valid direction.
refineRawNavCmd (RawNavCmd s []) = case strToDir s of
  Just dir -> Right (NavCmd dir)
  Nothing  -> Left $ InternalErr $ UnexpectedParams "refineRawNavCmd" [s]
-- Error cases.
refineRawNavCmd (RawNavCmd _ ps) =
  Left $ InternalErr $ UnexpectedParams "refineRawNavCmd" $ fmap show ps
refineRawNavCmd _ =
  Left $ InternalErr $ WrongRefine "refineRawNavCmd"

-- Interpreters for above commands, for all interpreter states: ---------------

navInterp :: Interp
navInterp cmd@(NavCmd dir) mrel st
    -- Navigation commands are only available when in TRANS/TRANS_SCRIPT state.
  | isTransState st =
     modifyTransEnv $ \transEnv ->
      let t = getTerm transEnv
          p = getPath transEnv
          -- Get term/path and try to amend the path according to specified
          -- nav. command. Check if the result is valid.
       in case transExec (navigateUT dir p) t of
           Left err -> outputCmdError err >> return transEnv
           Right p  -> return (update (cmd, mrel) (putPath p) transEnv)

    -- Wrong state.
  | otherwise = outputCmdError (StateErr st)

-- Anything else is an error.
navInterp _ _ _  = outputCmdError $ InternalErr $ WrongInter "navInterp"
