
module CmdRelCheck
  ( cmdRel       -- Lookup table for assigning relations to commands.
  , cmdRelCheck  -- Performs the relation checking.
  ) where

import CmdAST          (RawCmd(..), UnsafeRawCmd, SafeRawCmd)
import CmdError        (CmdError(..))
import KureCmdSettings (relLookup, kureCmdSettings)
import PPLib           (strArr)
import Relations       (Relation(..), impl)

import Control.Monad (msum)

{-
  Information:
  -----------------------------------------------------------------------------
 - Check if a command is safe to apply by ensuring its corresponding
   relation implies the global relation;
 - Two side notes:
   (1) The global relation may not be set as we have an experimental option
       that allows users to apply transformations however they like;
   (2) Commands may /not/ have a corresponding relation e.g., shell/state
       commands as they update the interactive environment etc. and
       do /not/ affect the "proof state".
-}

-- Lookup table for relations
cmdRel :: UnsafeRawCmd -> Maybe Relation
cmdRel RawNavCmd{}        = (Just R_EQ)
cmdRel RawAssumptionCmd{} = (Just R_EQ)  -- they are essentially 'axioms'.
cmdRel RawShellCmd{}      = Nothing
cmdRel RawBaseLibCmd{}    = Nothing
cmdRel RawStateCmd{}      = Nothing
cmdRel RawTransEnvCmd{}   = Nothing
cmdRel RawScriptCmd{}     = Nothing

-- Default to undefined relation if not in list, this will
-- simply prevent the command from being applied.
cmdRel (RawKureCmd s _) = msum [ relLookup s kureCmdSettings, Just R_UNDEFINED ]

{-
  - Check if a command is safe w.r.t. the global relation
  - A command is unsafe before it is checked and safe afterwards
  - We return the RawCmd with its relation as 'evidence' of it's validity
    in the current proof context. These are then logged in the
    transformation's history for reviewing/exporting later.
-}
cmdRelCheck :: UnsafeRawCmd
               -> Maybe Relation
               -> Maybe Relation
               -> Either CmdError (SafeRawCmd, Maybe Relation)
cmdRelCheck cmd Nothing    _        = Right (cmd, Nothing)
cmdRelCheck cmd mrel       Nothing  = Right (cmd, mrel)
cmdRelCheck cmd (Just rel) (Just grel)
  | impl rel grel = Right (cmd, Just rel)
  | otherwise     = Left $ RelationErr $ show rel ++ show strArr ++ show grel