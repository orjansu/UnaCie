{-# LANGUAGE LambdaCase #-}

module Inter where

import CmdAST          (Cmd(..), CmdName, SafeRawCmd, UnsafeRawCmd)
import CmdError        (CmdError(..), InternalError(..))
import CmdRefine       (refineRawCmd)
import Relations       (Relation)
import CmdRelCheck     (cmdRelCheck, cmdRel)
import InterState      (InterState(..))
import CmdParser       (parse') -- parse
import InterEnv        ( InterEnv, initInterEnv
                       , mTransEnv )
import InterPrintUtils (terminalAddTitleFrame, colouredFancyHeader)
import InterPrintUtils ( transPrompt, scriptPrompt
                       , transScriptPrompt )
import Types           (Matcher)
import InterUtils      ( exitActiveScript
                       , fromScriptState
                       , getActiveScriptCmd
                       , getCurrActiveScript
                       , getGRel
                       , getInterEnv
                       , getInterState
                       , initSettings
                       , interact
                       , interGetLine
                       , InterM
                       , interPutCritError
                       , interPutInfo
                       , interPutStrLn
                       , modifyInterEnv
                       , nextActiveScriptCmd
                       , outputCmdError)

-- External interpreters and matchers
import NavCmd          (navInterp       , matchers, navCmds)
import ShellCmd        (shellInterp     , matchers, shellCmds)
import BaseLibCmd      (baseLibInterp   , matchers, baseLibCmds)
import StateCmd        (stateInterp     , matchers, stateCmds)
import TransEnvCmd     (transEnvInterp  , matchers, transEnvCmds)
import ScriptCmd       (scriptInterp    , matchers, scriptCmds)
import AssumptionCmd   (assumptionInterp, matchers, assumptionCmds)
import KureCmdSettings ( interpLookup
                       , kureCmdSettings
                       , allMatchers
                       , allCmdNames )

import Prelude hiding                   (interact, init)
import Control.Monad                    ((>=>))
import System.Console.Haskeline         (modifyHistory)
import System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)
import System.Console.ANSI              (clearScreen)

{-
  <TO-DO>: - Silence terminal output for scripts/initialisation loop?

  Information:
  -----------------------------------------------------------------------------
  - Top-level interpreter function;
  - Handles all necessary interactions with the user;
  - InterM = Haskeline's InputT wrapped around state and IO.
-}

-- Top-level functions: -------------------------------------------------------

main ::  IO ()
main  =  clearScreen

          >> intro

          -- >> todo
          >> interact run (initSettings commands) initInterEnv

-- Introduction and helpful information
intro ::  IO ()
intro  =  mapM_ putStrLn colouredFancyHeader
         -- >> putStrLn "\ntype help if you need it!"

-- My <TO-DO> list
todo ::  IO ()
todo  =  mapM_ putStrLn $ terminalAddTitleFrame "<TO-DO> !IMPORTANT!"
          [ "view history as seen on screen using path projections"
          , "cmd-rel option for printing to command line/file (from cmd hist)"
          , "clean up error messages for ltoks on parse script"
          , "merge (cmd, rel) type to cmdRel"
          , "end goal options not done for trans state"
          , "trans goal should be alpha-equiv"
          , "use script parser to allow ; sepped input on command line for multi commands"
          , "back-hist in script mode should put prev-cmd"
          , "disallow terms/ctxs with free vs"
          , "combine functions for safeUnapplyCtxDefR"
          , "safety checks on script imports to avoid infinite loops"
          , "need different delimiters for pctx/pterm and ctx/term src code"
          , "let bindings print y x = .. but don't parse that way?"
          , "apply-def should fail if nothing to apply"
          , "what to do about free variables?!"
          , "history output needs re-deggaring"
          , "noticed a lex error for underscores!!"
          , "bug with comments on script lines when parsing"
          , "print if trans goal was reached"
          ]

-- Top-level interactive function: decides what prompt to print based
-- on the InterState, takes input from the user, interprets it, and loops
-- (transPrompt/scriptPrompt/transScriptPrompt in InterPrintUtils.hs)
run ::  InterM InterEnv ()
run  =  getInterState >>= \case
         INITIAL      -> interp
         TRANS        -> maybe interp transInterp
                          =<< transPrompt
         SCRIPT       -> scriptPrompt >> interp
         TRANS_SCRIPT -> maybe interp transInterp
                          =<< transScriptPrompt

        -- Loop in updated environment
        >> run
 where

     -- Misc: -----------------------------------------------------------------

     -- Two different input /prompts/ for non-trans/trans states
     interp        = interGetLine "\ESC[38;5;15munie> \ESC[m"
                      >>= addToHistory >>= interpInp
     transInterp i = interGetLine ("\ESC[38;5;15m[" ++ show i ++ "]> \ESC[m")
                      >>= addToHistory >>= interpInp

     -- Add user input to history file
     addToHistory Nothing       = return Nothing
     addToHistory inp@(Just "") = return inp
     addToHistory inp@(Just s)  = modifyHistory (addHistoryUnlessConsecutiveDupe s)
                                   >> return inp

     -- Interpreting user input: ----------------------------------------------

     -- Interpret command line input
     interpInp         :: Maybe String -> InterM InterEnv ()
     interpInp Nothing  = interPutCritError "interGetLine returned Nothing."
     interpInp (Just s) = getInterState >>= \st -> maybe interp id (lookup (st, s)
                           specialInterps)
                           -- Load non-special interpreter
                           where -- interp = getInterEnv (mTransEnv >=> getGRel)
                                 --         >>= \mRel -> either outputCmdError interpCmd
                                 --          (flip prepCmd mRel =<< parse gMatchers s)

                                 interp :: InterM InterEnv ()
                                 interp = case parse' gMatchers s of
                                   Left errs -> mapM_ (interPutStrLn . (++"hej") . show) errs
                                   Right cmds  -> runCmds cmds

                                 runCmds :: [UnsafeRawCmd] -> InterM InterEnv ()
                                 runCmds [] = return ()
                                 runCmds (c : cs) = runCmd c >> runCmds cs


                                 runCmd :: UnsafeRawCmd -> InterM InterEnv ()
                                 runCmd ucmd = getInterEnv (mTransEnv >=> getGRel)
                                            >>= \mRel -> either outputCmdError interpCmd
                                             (prepCmd ucmd mRel)

     -- Interpreters for special command line input
     specialInterps ::  [((InterState, String), InterM InterEnv ())]
     specialInterps  =  [ ((INITIAL     , "") , return ())
                        , ((SCRIPT      , "") , execScriptRawCmd)
                        , ((TRANS       , "") , return ())
                        , ((TRANS_SCRIPT, "") , execScriptRawCmd)
                        , ((INITIAL     , "!"), outputCmdError $ StateErr INITIAL)
                        , ((SCRIPT      , "!"), execAllScriptRawCmds)
                        , ((TRANS       , "!"), outputCmdError $ StateErr INITIAL)
                        , ((TRANS_SCRIPT, "!"), execAllScriptRawCmds)
                        ]

    -- Interpreting commands in the current environment
     interpCmd            :: (Cmd, Maybe Relation) -> InterM InterEnv ()
     interpCmd (cmd, mrel) = getInterState >>= \st ->
                             case cmd of
                              NavCmd{}        -> navInterp        cmd mrel st
                              ShellCmd{}      -> shellInterp      cmd mrel st
                              BaseLibCmd{}    -> baseLibInterp    cmd mrel st
                              TransEnvCmd{}   -> transEnvInterp   cmd mrel st
                              AssumptionCmd{} -> assumptionInterp cmd mrel st
                              StateCmd{}      -> stateInterp      cmd mrel st gMatchers
                              ScriptCmd{}     -> scriptInterp     cmd mrel st gMatchers

                              KureCmd s _   -> case interpLookup s kureCmdSettings of
                                                Just f  -> f cmd mrel st
                                                Nothing -> (outputCmdError
                                                             . InternalErr
                                                             . NoInter
                                                             . show) cmd

     -- Executing raw commands from scripts: ----------------------------------

     -- Execute the next raw command from the active script
     execScriptRawCmd :: InterM InterEnv ()
     execScriptRawCmd  = getInterEnv getActiveScriptCmd >>= \case
                       Nothing  -> interPutInfo "end of script."
                       -- Update to next command /first/ in case new script is loaded
                       Just cmd -> modifyInterEnv nextActiveScriptCmd >>
                                   (getInterEnv (mTransEnv >=> getGRel) >>= \mRel ->
                                    either outputCmdError interpCmd (prepCmd cmd mRel))

     -- Execute all remaining raw commands from the active script(s)
     execAllScriptRawCmds :: InterM InterEnv ()
     execAllScriptRawCmds  = getInterEnv getCurrActiveScript >>= \case
                           Nothing -> interPutInfo "all commands executed... \
                                       \exiting script(s)." >> fromScriptState
                           Just (s, _, i)
                            | i < length s -> execScriptRawCmd >> execAllScriptRawCmds
                            | otherwise    -> modifyInterEnv exitActiveScript
                                               >> execAllScriptRawCmds


-- Helpers: -------------------------------------------------------------------

{- Attempt to prepare a raw command to be executed:
   (1) Perform the inequational check against the global relation (if set);
   (2) Refine the raw command into a command that can be executed by some
       interpreter.
   Both can fail and return a CmdError.

   (Raw command is /unsafe/ before inequational check, and /safe/ afterwards)
-}
prepCmd            ::  UnsafeRawCmd
                       -> Maybe Relation
                       -> Either CmdError (Cmd, Maybe Relation)
prepCmd ucmd mgrel  =  do
                         (scmd, mrel) <- ineqCheck ucmd mgrel -- (1)
                         cmd          <- refineCmd scmd       -- (2)
                         return (cmd, mrel)
                       where
                            -- (1) Inequational check
                            ineqCheck :: UnsafeRawCmd
                                         -> Maybe Relation
                                         -> Either CmdError (SafeRawCmd, Maybe Relation)
                            ineqCheck ucmd = cmdRelCheck ucmd (cmdRel ucmd)

                            -- (2) Refine raw command
                            refineCmd :: SafeRawCmd -> Either CmdError Cmd
                            refineCmd  = refineRawCmd

-- Global set of matchers: used for parsing commands from the command line/scripts.
gMatchers ::  [Matcher]
gMatchers  =  NavCmd.matchers
               ++ ShellCmd.matchers
               ++ BaseLibCmd.matchers
               ++ StateCmd.matchers
               ++ TransEnvCmd.matchers
               ++ ScriptCmd.matchers
               ++ AssumptionCmd.matchers
               -- All matchers for KURE commands
               ++ allMatchers kureCmdSettings

-- Complete list of commands for auto-completion.
commands :: [CmdName]
commands  = allCmdNames kureCmdSettings
            ++ transEnvCmds
            ++ assumptionCmds
            ++ baseLibCmds
            ++ navCmds
            ++ scriptCmds
            ++ shellCmds
            ++ stateCmds
            ++ baseLibCmds
