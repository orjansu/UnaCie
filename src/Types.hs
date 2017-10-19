
module Types
  ( Interp   -- The type of a command interpreter.
  , Matcher  -- The type of a command matcher.
  , Refiner  -- The type of a command refiner.
  ) where

import CmdAST      (Cmd, RawCmd, UnsafeRawCmd)
import CmdError    (CmdError)
import CmdLexUtils (LocatedToken)
import InterEnv    (InterEnv)
import InterState  (InterState)
import InterUtils  (InterM)
import Relations   (Relation)

{-
  Information:
  -----------------------------------------------------------------------------
  - A number of key types used throughout different modules. 
-}

{-
  - The type of a command interpreter;
  - Takes a command, its associated relation (if applicable), UNIE's current
    state, and outputs an interactive computation in a (potentially updated)
    environment.
-}
type Interp = Cmd
              -> Maybe Relation
              -> InterState
              -> InterM InterEnv ()

{-
  - The type of a command matcher;
  - Takes a list of located tokens and attempts to construct an unsafe raw
    command. The list output corresponds to multiple matching options, 
    which is due to the fact that commands may have different parameter
    options.
-}
type Matcher = [LocatedToken] -> [Either CmdError UnsafeRawCmd]

{- 
  - The type of a command refiner;
  - Takes a raw command and attempts to refine it to a fully-fledged command
    by performing additional validation checks on its parameter(s);
  - Returns a command error if the raw command is invalid.
-}
type Refiner = RawCmd -> Either CmdError Cmd