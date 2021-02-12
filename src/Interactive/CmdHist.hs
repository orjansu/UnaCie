
module CmdHist
  ( CmdHist       -- List of commands and their corresponding relations.
  , emptyCmdHist  -- Initial settings.
  , steps         -- Number of command steps in history.
  , navSteps      -- Number of navigation command steps in history.
  ) where

import CmdAST    (Cmd(..))
import Relations (Relation)

{-
  <TO-DO>: Should we make a separate command history for all successfully
           executed commands, regardless of if they affect the proof
           state?

  Information:
  -----------------------------------------------------------------------------
  - The command history stores commands executed that affect the proof state
    and their corresponding relation;
  -- Not all commands executed by the interpreter are stored, for example,
     state/shell commands are not.
  - This history can then be reported to the user or exported as a command
    script to be re-executed in the future.
-}

type CmdHist = [(Cmd, Maybe Relation)]

-- Initial settings: ----------------------------------------------------------

emptyCmdHist :: CmdHist
emptyCmdHist  = []

-- Helpers: -------------------------------------------------------------------

-- Number of command steps stored in history.
steps :: CmdHist -> Int
steps  = length

-- Number of navigation commands stored in history.
navSteps :: CmdHist -> Int
navSteps  = length . filter (\(cmd, _) -> isNav cmd)
            where
              isNav NavCmd{} = True
              isNav _        = False
