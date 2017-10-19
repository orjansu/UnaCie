{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , LambdaCase #-}

module InterUtils where

import CtxAST        (GBind(..), Ctx, Term, Name)
import CtxPatAST     (CtxPat)
import CmdAST        (Cmd, RawCmd, Script, UnsafeRawCmd)
import CtxKind       (CtxKind)
import CtxEqLib      ( CtxEqLib, emptyCtxEqLib, insertCtxEqs
                     , deleteCtxEqs, deleteCtxEq )
import Crumb         (Crumb)
import CmdError      (CmdError(..))
import CmdLexUtils 
import CmdParser
import Relations     (Relation)
import Universes     (U)
import KureMonad     (T, R, sv2KureMEnv)
import InterState    (InterState(..))
import TransHist     (TransHist(..))
import FileUtils     (exportToFile, parseScriptFile)
import Utils         (safeHead, safeTail, notNull, prepStr)
import PrintSettings (terminalLineWidth)
import IndentationParserLib (runParser, integer)
import KureExtra
import InterEnv
import NavSettings
import PPLib
import qualified InterState as IS
import qualified Runners    as R

import Prelude hiding            (interact, log)
import Control.Monad.Trans.Class (lift)
import Control.Monad             ((<=<), (>=>))
import Control.Monad.IO.Class    (liftIO)
import Data.List                 (isPrefixOf)
import Data.Maybe                (isJust)
import Data.Bifunctor            (bimap)
import Language.KURE
import System.Console.Haskeline
import qualified Control.Monad.State.Strict as ST
import qualified Data.Map                   as Map

{-
  <TO-DO>: - KURE transformation helpers have bad function names, maybe better
             in a different module?

  Information:
  -----------------------------------------------------------------------------
  - InterM = interactive monad. This is state + IO with Haskeline's InputT
    wrapped around it;
  - InterM functions in this module abstract out common InterM requirements,
    including:
    - Functions that access/update/modify InterM's state (InterEnv);
    - Helper functions associated with such interactions that bridge the gap
      between actual user input and the task of modifying the InterEnv;
    - Functions necessary for IO interactions, such as print errors/warnings
      to the console.
-}

-------------------------------------------------------------------------------
-- Interpreter monad: --
-------------------------------------------------------------------------------

type InterM s a = InputT (ST.StateT s IO) a

instance ST.MonadState s (InputT (ST.StateT s IO)) where
  get  =  lift ST.get
  put  =  lift . ST.put

-- Top-level function for interaction
interact                ::  MonadException m
                            => InputT (ST.StateT s m) a
                            -> Settings (ST.StateT s m)
                            -> s
                            -> m a
interact m settings      =  ST.evalStateT (runInputT settings m)

-- Initial settings: ----------------------------------------------------------

initSettings ::  Monad m => [String] -> Settings (ST.StateT s m)
initSettings cmds =  Settings { historyFile = Just "command_history"
                          , complete = completeWord Nothing " \t" $
                             return . completer cmds
                          , autoAddHistory = False
                          }

-- Command completer
completer           ::  [String] -> String -> [Completion]
completer cmds str   =  simpleCompletion <$> filter
                         (str `isPrefixOf`) cmds

-------------------------------------------------------------------------------
-- IO interactions: --
-------------------------------------------------------------------------------

-- Printing: (-2 for surrounding spaces)

interPutCritError   ::  String -> InterM s ()
interPutCritError    =  mapM_ interPutStrLn
                        . fmap (\s -> "\ESC[30m\ESC[1m\ESC[48;5;160m "
                                      ++ s ++ " \ESC[m")
                        . squashString (terminalLineWidth - 2)
                        . ("Critical error: " ++)

interPutError       ::  String -> InterM s ()
interPutError        =  mapM_ interPutStrLn
                        . fmap (\s -> "\ESC[37m\ESC[41m "
                                      ++ s ++ " \ESC[m")
                        . squashString (terminalLineWidth - 2)
                        . ("Error: " ++)

interPutWarning     ::  String -> InterM s ()
interPutWarning      =  mapM_ interPutStrLn
                        . fmap (\s -> "\ESC[30m\ESC[48;5;215m "
                                      ++ s ++ " \ESC[m")
                        . squashString (terminalLineWidth - 2)
                        . ("Warning: " ++)

interPutInfo        ::  String -> InterM s ()
interPutInfo         =  mapM_ interPutStrLn
                        . fmap (\s -> "\ESC[37m\ESC[48;5;6m " ++ s
                                       ++ " \ESC[m")
                        . squashString (terminalLineWidth - 2)
                        . ("Info: " ++)

interDebug          ::  String -> InterM s ()
interDebug           =  mapM_ interPutStrLn
                        . fmap (\s -> "\ESC[30m\ESC[1m\ESC[48;5;160m "
                                      ++ s ++ " \ESC[m")
                        . squashString (terminalLineWidth - 2)
                        . ("Debug: " ++)

interPrint          ::  Show a => a -> InterM s ()
interPrint           =  interPutStrLn . show

interPutStrLn       ::  String -> InterM s ()
interPutStrLn        =  outputStrLn

interPutStr         ::  String -> InterM s ()
interPutStr          =  outputStr

-- Getting
interGetLine        ::  String -> InterM s (Maybe String)
interGetLine         =  getInputLine

-- Outputting command errors: --

outputCmdError  ::  CmdError -> InterM s ()
outputCmdError   =  interPrint

outputCmdErrors ::  [CmdError] -> InterM s ()
outputCmdErrors  =  mapM_ interPrint

outputLibNotFoundError ::  String -> InterM s ()
outputLibNotFoundError  =  outputCmdError . LibErr
                            . (\s -> '\'' : s ++ "' not found.")

outputLibNotFoundError' :: String -> InterM s ()
outputLibNotFoundError'  = interPutError . (\s -> '\'' : s ++ "' not found in library.")

-------------------------------------------------------------------------------
-- Getters/setters/modifiers: --
-------------------------------------------------------------------------------

-- InterEnv: ------------------------------------------------------------------

getInterEnv    ::  (InterEnv -> a) -> InterM InterEnv a
getInterEnv     =  ST.gets

putInterEnv    ::  InterEnv -> InterM InterEnv ()
putInterEnv     =  ST.put

modifyInterEnv ::  (InterEnv -> InterEnv) -> InterM InterEnv ()
modifyInterEnv  =  ST.modify

-- BaseLib: -------------------------------------------------------------------

getBaseLib             ::  InterEnv -> BaseLib
getBaseLib              =  baseLib

putBaseLib             ::  BaseLib -> InterEnv -> InterEnv
putBaseLib lib env      =  env { baseLib = lib }

-- GBinds
insertGBinds  ::  [GBind] -> InterEnv -> InterEnv
insertGBinds gs env
 =  (putCBinds cBinds' . putTBinds tBinds') env
    where
         (cBinds', tBinds') = foldr ins (getCBinds env, getTBinds env) gs

         ins (CBind k ns ctx) (c, t) = (Map.insert ns (k, ctx) c, t)
         ins (TBind ns term)  (c, t) = (c, Map.insert ns term t)

-- CBinds
getCBinds              ::  InterEnv -> Map.Map Name (CtxKind, Ctx)
getCBinds               =  cBinds . getBaseLib

getCBinds'             ::  BaseLib -> [GBind]
getCBinds'              =  fmap (\(n, (k, c)) -> CBind k n c)
                           . Map.toList
                           . cBinds

putCBinds              ::  Map.Map Name (CtxKind, Ctx) -> InterEnv -> InterEnv
putCBinds bs env        =  env { baseLib = (getBaseLib env) { cBinds = bs } }

-- TBinds
getTBinds              ::  InterEnv -> Map.Map Name Ctx
getTBinds               =  tBinds . getBaseLib

getTBinds'             ::  BaseLib -> [GBind]
getTBinds'              =  fmap (uncurry TBind) . Map.toList . tBinds

putTBinds              ::  Map.Map Name Ctx -> InterEnv -> InterEnv
putTBinds bs env        =  env { baseLib = (getBaseLib env) { tBinds = bs } }

-- CtxEqs
getCtxEqs              ::  InterEnv -> CtxEqLib
getCtxEqs               =  ctxEqLib . getBaseLib

putCtxEqs              ::  CtxEqLib -> InterEnv -> InterEnv
putCtxEqs lib env       =  env { baseLib = (getBaseLib env) { ctxEqLib = lib } }

insertCtxEqss          ::  [([CtxPat], CtxKind)] -> InterEnv -> InterEnv
insertCtxEqss xs env    =  putCtxEqs ctxEqs' env
                           where ctxEqs' = foldr (uncurry insertCtxEqs)
                                  (getCtxEqs env) xs

deleteCtxEqs  ::  CtxKind -> InterEnv -> InterEnv
deleteCtxEqs k env
 =  env { baseLib = base { ctxEqLib = CtxEqLib.deleteCtxEqs k ctxEqs } }
    where
         ctxEqs = ctxEqLib base
         base   = getBaseLib env

deleteCtxEq  ::  CtxKind -> Int -> InterEnv -> Maybe InterEnv
deleteCtxEq k i env
  =  fmap (\ctxEqs' -> env { baseLib = base { ctxEqLib =  ctxEqs' } })
       (CtxEqLib.deleteCtxEq k i ctxEqs)
     where
          ctxEqs = ctxEqLib base
          base   = getBaseLib env

-- Terms
lookupTerm             ::  Name -> InterEnv -> Maybe Term
lookupTerm ns           =  Map.lookup ns . tBinds . getBaseLib

lookupTerm'            ::  Name -> BaseLib -> Maybe Term
lookupTerm' ns          =  Map.lookup ns . tBinds

checkTerm              ::  Name -> InterEnv -> Bool
checkTerm ns            =  isJust <$> lookupTerm ns

deleteTerms            ::  InterEnv -> InterEnv
deleteTerms             =  putTBinds mempty

deleteTerm             ::  Name -> InterEnv -> Maybe InterEnv
deleteTerm ns env       =  if checkTerm ns env
                              then Just env { baseLib = base
                                    { tBinds = Map.delete ns ts } }
                              else Nothing
                           where
                                ts   = tBinds base
                                base = getBaseLib env
-- Ctxs
lookupCtx              ::  Name -> InterEnv -> Maybe (CtxKind, Ctx)
lookupCtx ns            =  Map.lookup ns . cBinds . getBaseLib

lookupCtx'             ::  Name -> BaseLib -> Maybe (CtxKind, Ctx)
lookupCtx' ns           =  Map.lookup ns . cBinds

checkCtx               ::  Name -> InterEnv -> Bool
checkCtx ns             =  isJust <$> lookupCtx ns

deleteCtxs             ::  InterEnv -> InterEnv
deleteCtxs              =  putCBinds mempty

deleteCtxEqss          ::  InterEnv -> InterEnv
deleteCtxEqss           =  putCtxEqs emptyCtxEqLib

deleteCtx              ::  Name -> InterEnv -> Maybe InterEnv
deleteCtx ns env        =  if checkCtx ns env
                              then Just env { baseLib = base
                                    { cBinds = Map.delete ns cs } }
                              else Nothing
                           where
                                cs   = cBinds base
                                base = getBaseLib env

-- InterState: ----------------------------------------------------------------

getInterState          ::  InterM InterEnv InterState
getInterState           =  ST.gets state

putInterState          ::  InterState -> InterEnv -> InterEnv
putInterState st env    =  env { state = st }

modifyInterState       ::  (InterState -> InterState) -> InterEnv -> InterEnv
modifyInterState f env  =  env { state = f (state env) }

resetInterState        ::  InterEnv -> InterEnv
resetInterState env     =  env { state = INITIAL }

-- TransEnv: ------------------------------------------------------------------

getMTransEnv             ::  InterEnv -> Maybe TransEnv
getMTransEnv              =  mTransEnv

-- These are lifted into InterM to deal with the possibility of TransEnv
-- not being set (i.e., Nothing)
modifyTransEnv           ::  (TransEnv -> InterM InterEnv TransEnv)
                             -> InterM InterEnv ()
modifyTransEnv f          =  maybe transEnvCritError
                              (f >=> modifyInterEnv . putTransEnv)
                              =<< getInterEnv getMTransEnv

getTransEnv              ::  (TransEnv -> InterM InterEnv ())
                             -> InterM InterEnv ()
getTransEnv f             =  maybe transEnvCritError f
                              =<< getInterEnv getMTransEnv

putTransEnv              ::  TransEnv -> InterEnv -> InterEnv
putTransEnv transEnv env  =  env { mTransEnv = Just transEnv }

nullTransEnv             ::  InterEnv -> InterEnv
nullTransEnv env          =  env { mTransEnv = Nothing }

-- Critical error if state transitions go wrong
transEnvCritError        ::  InterM InterEnv ()
transEnvCritError         =  interPutCritError "expected \
                              \transformation state, \
                              \resetting state..."
                             >> fromTransState

-- Critical error if can't project node at current path 
pathCritError            ::  InterM InterEnv ()
pathCritError             =  interPutCritError "can't display \
                              \focussed node, resetting state..."
                             >> fromTransState                          

-- Term: --

getTerm                ::  TransEnv -> Term
getTerm                 =  term

putTerm                ::  Term -> TransEnv -> TransEnv
putTerm t env           =  env { term = t }

-- Path: --

getPath                ::  TransEnv -> AbsolutePath Crumb
getPath                 =  path

putPath                ::  AbsolutePath Crumb -> TransEnv -> TransEnv
putPath p env           =  env { path = p }

nullPath               ::  TransEnv -> TransEnv
nullPath                =  putPath mempty

-- Safe variables: --

getSVs                 ::  TransEnv -> [Name]
getSVs                  =  svs

putSVs                 ::  [Name] -> TransEnv -> TransEnv
putSVs nss env          =  env { svs = nss }

-- Current relation: --

getCRel                ::  TransEnv -> Maybe Relation
getCRel                 =  cRel

putCRel                ::  Maybe Relation -> TransEnv -> TransEnv
putCRel mrel env        =  env { cRel = mrel }

-- Global relation: --

getGRel                ::  TransEnv -> Maybe Relation
getGRel                 =  gRel

putGRel                ::  Maybe Relation -> TransEnv -> TransEnv
putGRel mrel env        =  env { gRel = mrel }

-- Goal: --

getGoal                ::  TransEnv -> Maybe Term
getGoal                 =  goal

putGoal                ::  Maybe Term -> TransEnv -> TransEnv
putGoal mt env          =  env { goal = mt }

-- TransHist: -

getHist                ::  TransEnv -> TransHist
getHist                 =  hist

putHist                ::  TransHist -> TransEnv -> TransEnv
putHist h env           =  env { hist = h }

modHist                ::  (TransHist -> TransHist) -> TransEnv -> TransEnv
modHist f env           =  env { hist = f (hist env) }

-- Logging commands in TransHist
log  ::  (Cmd, Maybe Relation) -> TransEnv -> TransEnv
log cmr env
 =  putHist (hist { stateHist = (getTerm env,
                                 getPath env) : stateHist hist
                  , cmdHist   = cmr : cmdHist hist
                  }) env
    where hist = getHist env

-- Logging command updates to TransHist
update                       ::  (Cmd, Maybe Relation)
                                 -> (TransEnv -> TransEnv)
                                 -> TransEnv
                                 -> TransEnv
update (cmd, Nothing)  f env  =  log (cmd, Nothing)  (f env)
update (cmd, Just rel) f env  =  log (cmd, Just rel) (f env')
                                 where env' = putCRel (Just rel) env

-- Reverting TransHist
revertHist  ::  TransEnv -> TransEnv
revertHist env
 =  case stateHist hist of
     []               -> env
     [_]              -> env
     -- These should be updated in lockstep, but just in case we use safeTail
     (_: (t, p) : hs) -> let hist' = hist { stateHist = (t, p) : hs
                                          , cmdHist   = safeTail (cmdHist hist) }
                             in (putTerm t
                                 . putPath p
                                 . putHist hist') env
    where hist = getHist env

-- Initial term: --

getInitTerm            ::  TransEnv -> Term
getInitTerm             =  initTerm

putInitTerm            ::  Term -> TransEnv -> TransEnv
putInitTerm t env       =  env { initTerm = t }

-- Initial BaseLib: --

getInitBaseLib         ::  TransEnv -> BaseLib
getInitBaseLib          =  initBaseLib

putInitBaseLib         ::  BaseLib -> TransEnv -> TransEnv
putInitBaseLib lib env  =  env { initBaseLib = lib }

-- NavSettings: --

getNavSettings         ::  TransEnv -> NavSettings
getNavSettings          =  navSettings

putNavSettings         ::  NavSettings -> TransEnv -> TransEnv
putNavSettings ns env   =  env { navSettings = ns }

highlightNav           ::  AbsolutePath Crumb -> TransEnv -> TransEnv
highlightNav p env      =  env { navSettings = NavSettings.highlight p 
                            (navSettings env) }

unHighlightNav         ::  TransEnv -> TransEnv 
unHighlightNav env      =  env { navSettings = NavSettings.unHighlight 
                            (navSettings env) }

-- ScriptEnv: -----------------------------------------------------------------

-- Critical error if state transitions go wrong
scriptEnvCritError     ::  InterM InterEnv ()
scriptEnvCritError      =  interPutCritError "expected \
                            \script state... \
                            \resetting state."
                           >> fromScriptState

getScriptEnv           ::  InterEnv -> ScriptEnv
getScriptEnv            =  scriptEnv

putScriptEnv           ::  ScriptEnv  -> InterEnv -> InterEnv
putScriptEnv e env      =  env { scriptEnv = e }

-- Scripts: --

getScripts             ::  InterEnv -> Map.Map String Script
getScripts              =  scripts . getScriptEnv

addScript              ::  String -> Script -> InterEnv -> InterEnv
addScript ref s env     =  putScriptEnv scriptEnv { scripts
                            = Map.insert ref s loaded } env
                           where
                                loaded    = scripts scriptEnv
                                scriptEnv = getScriptEnv env

getScript              ::  String -> InterEnv -> Maybe Script
getScript s             =  Map.lookup s . scripts . getScriptEnv

loadScript  ::  String -> InterEnv -> Maybe InterEnv
loadScript ref env
 =  fmap (\s -> putScriptEnv scriptEnv { activeScripts = (s, ref, 0)
           : activeScripts scriptEnv } env) (Map.lookup ref loaded)
    where
         loaded    = scripts scriptEnv
         scriptEnv = getScriptEnv env

-- Active scripts: --

isActiveScript         ::  InterEnv -> Bool
isActiveScript          =  notNull . getActiveScripts

getActiveScripts       ::  InterEnv -> [(Script, String, Int)]
getActiveScripts        =  activeScripts . getScriptEnv

getCurrActiveScript    ::  InterEnv -> Maybe (Script, String, Int)
getCurrActiveScript     =  safeHead . activeScripts . getScriptEnv

reloadActiveScript     ::  InterEnv -> Maybe InterEnv
reloadActiveScript env
 =  case actives of
     [] -> Nothing
     ((s, ref, _) : xs) -> Just $ putScriptEnv scriptEnv
                            { activeScripts = (s, ref, 0) : xs } env
    where
         actives   = activeScripts scriptEnv
         scriptEnv = getScriptEnv env

exitActiveScript       ::  InterEnv -> InterEnv
exitActiveScript env    =  putScriptEnv scriptEnv { activeScripts =
                             safeTail actives } env
                           where
                                actives   = activeScripts scriptEnv
                                scriptEnv = getScriptEnv env

getActiveScriptCmd     ::  InterEnv -> Maybe RawCmd
getActiveScriptCmd env  =  safeHead actives >>= \(s, _, i) ->
                           if i >= length s
                              then Nothing
                              else Just (s !! i)
                           where actives = activeScripts (getScriptEnv env)

nextActiveScriptCmd  ::  InterEnv -> InterEnv
nextActiveScriptCmd env
 =  case actives of
     []                 -> env
     ((s, ref, i) : xs) -> if succ i > length s
                              then env
                              else putScriptEnv scriptEnv { activeScripts =
                                   (s, ref, succ i) : xs } env

    where
         actives   = activeScripts scriptEnv
         scriptEnv = getScriptEnv env

prevActiveScriptCmd  ::  InterEnv -> InterEnv
prevActiveScriptCmd env
 =  case actives of
     []                 -> env
     ((s, ref, i) : xs) -> if pred i < 0
                              then env
                              else putScriptEnv scriptEnv { activeScripts =
                                    (s, ref, pred i) : xs } env
    where
         actives   = activeScripts scriptEnv
         scriptEnv = getScriptEnv env

-------------------------------------------------------------------------------
-- Helpers: --
-------------------------------------------------------------------------------

-- TransEnv helpers: ----------------------------------------------------------

-- Options for when reached the goal state
endGoalOpts ::  InterM InterEnv ()
endGoalOpts  =  interPutStrLn "<TO-DO> give user end goal options."

-- Export helpers: ------------------------------------------------------------

baseLibExport       ::  FilePath
                        -> (BaseLib -> [String])
                        -> InterM InterEnv ()
baseLibExport fp f   =  maybe (interPutInfo "export complete.") outputCmdError
                         =<< liftIO . exportToFile fp . f
                         =<< getInterEnv getBaseLib

transEnvExport :: FilePath -> (TransEnv -> [String]) -> InterM InterEnv ()
transEnvExport fp f = getTransEnv $ maybe (interPutInfo "export complete.")
                       outputCmdError <=< (liftIO . exportToFile fp . f)

-- State changes: -------------------------------------------------------------

-- InterM versions of respective InterState commands

toTransState    ::  InterM InterEnv ()
toTransState     =  modifyInterEnv (modifyInterState IS.toTransState)

fromTransState  ::  InterM InterEnv ()
fromTransState   =  modifyInterEnv (modifyInterState IS.fromTransState)

toScriptState   ::  InterM InterEnv ()
toScriptState    =  modifyInterEnv (modifyInterState IS.toScriptState)

fromScriptState ::  InterM InterEnv ()
fromScriptState  =  modifyInterEnv (modifyInterState IS.fromScriptState)

-- KURE transformation helpers: -----------------------------------------------

-- Execute a KURE transformation and convert the error to a CmdError
transExec     ::  Injection a U => T U b -> a -> Either CmdError b
transExec t x  =  bimap KureErr id (R.applyTUInj t x)

-- Rewrite helpers: --

-- Use current path to apply a rewrite to a sub-term in the AST
applyRTermCurrPathLog  ::  (Cmd, Maybe Relation)
                           -> R Term
                           -> InterM InterEnv ()
applyRTermCurrPathLog log r
 =  modifyTransEnv $ \transEnv ->
    let t   = getTerm transEnv
        p   = getPath transEnv
        svs = getSVs  transEnv

    in case R.applyREnvCtx (extractR
                            . applyAtSnocPathR p
                            . promoteWithFailMsgR focussedErr
                            $ r)
                          (sv2KureMEnv svs) t of
        Left err   -> outputCmdError (KureErr err)
                       >> return transEnv
        Right term -> return $ update log
                           (putTerm term) transEnv
    where focussedErr = "attempting to apply a term rewrite when \
                         \focussed on different AST constructor e.g., \
                         \a let binding."

-- Use current path to apply a rewrite to a sub-term in the AST
-- Update function to handle result
applyRTermCurrPathUpdate  ::  R Term
                              -> (Term -> InterM InterEnv ())
                              -> InterM InterEnv ()
applyRTermCurrPathUpdate r f
 =  getTransEnv $ \transEnv ->
    let t   = getTerm transEnv
        p   = getPath transEnv
        svs = getSVs  transEnv

    in case R.applyREnvCtx (extractR
                            . applyAtSnocPathR p
                            . promoteWithFailMsgR focussedErr
                            $ r)
                           (sv2KureMEnv svs) t of
        Left err   -> outputCmdError (KureErr err)
        Right term -> f term
    where focussedErr = "attempting to apply a term rewrite when \
                        \focussed on different AST constructor e.g., \
                        \a let binding."

-- Apply a rewrite at the root, i.e., to the whole AST
applyRTermLog       ::  (Cmd, Maybe Relation) -> R Term -> InterM InterEnv ()
applyRTermLog log r  =  modifyTransEnv $ \transEnv ->
                        let t   = getTerm transEnv
                            svs = getSVs  transEnv
                        in case R.applyREnvCtx r (sv2KureMEnv svs) t of
                            Left err   -> outputCmdError (KureErr err)
                                           >> return transEnv
                            Right term -> return $ update log
                                           (putTerm term) transEnv

-- Apply a transformation at the current path 
-- Update function to handle result
applyTTermCurrPathUpdate :: T Term b 
                            -> (b -> InterM InterEnv ())
                            -> InterM InterEnv ()
applyTTermCurrPathUpdate t f
 =  getTransEnv $ \transEnv ->
    let term = getTerm transEnv
        p    = getPath transEnv
        svs  = getSVs  transEnv

    in case R.applyTEnvCtx (extractT
                            . applyAtSnocPathT p
                            . promoteWithFailMsgT focussedErr
                            $ t)
                           (sv2KureMEnv svs) term of 
        Left err   -> outputCmdError (KureErr err)
        Right term -> f term  

    where focussedErr = "attempting to apply a term rewrite when \
                        \focussed on different AST constructor e.g., \
                        \a let binding."



-- User choices: --------------------------------------------------------------

usrChoiceIdxWithCancel        ::  Int -> InterM InterEnv (Maybe Int)
usrChoiceIdxWithCancel maxIdx  =  go 
  where 
       go = fmap prepStr <$> interGetLine "> " >>= \case 
             Nothing       -> critErr >> return Nothing
             Just "cancel" -> return Nothing 
             Just inp      -> case runParser integer inp of 
                               Nothing -> inpErr >> go
                               Just n  -> if n >= 0 && n <= maxIdx 
                                             then return (Just n)
                                             else idxErr >> go
       inpErr  = interPutError "invalid input, please choose an index or 'cancel'."
       idxErr  = interPutError "invalid index, please choose again or 'cancel'."
       critErr = interPutCritError "interGetLine returned Nothing." 

-- Import a script from file, takes a name to use for storing it in the library.
importScript :: FilePath  -> String -> [[LocatedToken] -> [Either CmdError UnsafeRawCmd]] -> InterM InterEnv ()
importScript fp s matchers = 
  either outputCmdError (\script -> modifyInterEnv (addScript s script) 
    >> interPutInfo "library updated.")
  =<< liftIO (parseScriptFile fp $ parseScript matchers)