
module CmdRefine (refineRawCmd) where

import CmdAST   (Cmd, RawCmd(..), SafeRawCmd)
import CmdError (CmdError(..), InternalError(..))

-- External refiners
import BaseLibCmd      (refineRawBaseLibCmd)
import KureCmdSettings (refinerLookup, kureCmdSettings)
import NavCmd          (refineRawNavCmd)
import ScriptCmd       (refineRawScriptCmd)
import ShellCmd        (refineRawShellCmd)
import StateCmd        (refineRawStateCmd)
import TransEnvCmd     (refineRawTransEnvCmd)
import AssumptionCmd   (refineRawAssumptionCmd)

{-
  <TO-DO>: Think more about the ordering of command errors:
            (1) Lexing
            (2) Parsing
            (3) Relation
            (4) Refinement
           Should (3) come before (2)?

  Information:
  -----------------------------------------------------------------------------
  - The process of command refinement is to ensure a command's parameters are
    "semantically" correct;
  - The parsing process (using matchers) only ensures that a command's
    parameters are of the correct basic /type/, but as we only have a small
    set of tokens, we need an additional layer of checking, which refinement
    provides;
  - It would be possible to merge this refinement process with the parsing
    process, however, we wish to place a greater emphasis on when it is /safe/
    to apply transformation commands (i.e., those that affect the proof
    state), rather than first ensuring their parameters are in fact
    "semantically" correct. This is in part due to the complexity of context
    matching and/or generation, which frequently occurs in the refinement
    stage.
  - Hence we have the following ordering on the occurrence of command errors:
     (1) Lexing        -> a command cannot be made sense of
     (2) Parsing       -> a command's parameters are the wrong type/
                          missing/additional parameters
     (3) Relation      -> a command cannot safely be applied
     (4) Refinement    -> a command's parameters are semantically incorrect
  - In an equational setting, we feel (3) is more important than (4).
  -- In the future, I wish to consider whether (3) is actually more important
     than (2), but for now I prefer users to become familiar with interacting
     with UNIE itself.
  - Command refinement does not take into account the context in which the
    command will be executed, thus further errors can arise after this
    process is completed. For example, the command:

      > show-lib 'reverse

    would fail if the term 'reverse' does not appear in the interactive
    environment. It is the responsibility of the command interpreters to
    handle such context-dependent errors.

 - Basic example of command refinement:

     > show-lib foo

   (1) The BaseLib matchers recognise 'show-lib' as a valid command and 'foo'
       a CmdName token, which is listed as a valid parameter /type/ for the
       respective command. Thus the command is parsed successfully.
   (2) The refinement process rejects 'foo', as it is /not/ a valid parameter
       of 'show-lib'. A parameter error is returned.
-}

-- Notice the incoming command is /safe/, which indicates that
-- it has been CmdRelCheck-ed
refineRawCmd :: SafeRawCmd -> Either CmdError Cmd
refineRawCmd cmd@RawNavCmd{}        = refineRawNavCmd        cmd
refineRawCmd cmd@RawShellCmd{}      = refineRawShellCmd      cmd
refineRawCmd cmd@RawBaseLibCmd{}    = refineRawBaseLibCmd    cmd
refineRawCmd cmd@RawTransEnvCmd{}   = refineRawTransEnvCmd   cmd
refineRawCmd cmd@RawStateCmd{}      = refineRawStateCmd      cmd
refineRawCmd cmd@RawScriptCmd{}     = refineRawScriptCmd     cmd
refineRawCmd cmd@RawAssumptionCmd{} = refineRawAssumptionCmd cmd 
refineRawCmd cmd@(RawKureCmd s _)   = case refinerLookup s kureCmdSettings of
  Just f  -> f cmd
  Nothing -> Left . InternalErr . NoRefine . show $ cmd