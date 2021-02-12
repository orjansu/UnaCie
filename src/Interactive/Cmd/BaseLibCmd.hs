{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module BaseLibCmd
  ( matchers             -- Command line matchers for valid base lib. commands.
  , refineRawBaseLibCmd  -- Refiner for raw base lib. commands.
  , baseLibCmds          -- All base lib. commands.
  , baseLibInterp        -- Interpreter for base lib. commands.
  ) where

import CmdAST           (Cmd(..), Param(..), RawCmd(..))
import CmdError         (CmdError(..), InternalError(..))
import CmdParser        (cmdMatcher)
import CtxAST           (Ctx(..), GBind(..))
import CtxCheck         (checkCtxKind)
import CtxParser        (gBinds)
import CtxPatAST        (UPat(..))
import CtxPatParser     (ctxs)
import Display          (display)
import FileUtils        (parseFile')
import InterEnv         (InterEnv)
import InterPrintUtils  ( ctxEqsToTerminal, ctxEqLibToTerminal, ctxEqsToFile
                        , displayLibNames, mapKeysToTerminal'
                        , termsCtxsLibToFile )
import InterState       (isTransState)
import PPLib            (ppr)
import ParamParser      ( cmdNameMatcher, fileMatcher, numberMatcher
                        , srcCodeMatcher, srcNameMatcher )
import ParamRefine      ( cBindSrcCodeRefine, cVarSrcCodeRefine
                        , cmdNameRefine, ctxKindRefine, ctxPatSrcCodeRefine
                        , ctxSrcNameRefine, fileRefine, numberRefine
                        , paramsRefine, tBindSrcCodeRefine
                        , termSrcNameRefine )
import PrintSettings    (terminalLineStyle)
import Subst            (delCtxSubst)
import Types            (Interp, Matcher, Refiner)
import Universes        (U(..))
import InterUtils
  ( InterM
  , applyRTermCurrPathLog
  , applyRTermCurrPathUpdate
  , applyTTermCurrPathUpdate
  , baseLibExport
  , deleteCtx
  , deleteCtxEq
  , deleteCtxEqs
  , deleteCtxEqss
  , deleteCtxs
  , deleteTerm
  , deleteTerms
  , getCBinds
  , getCtxEqs
  , getInterEnv
  , getTBinds
  , insertCtxEqss
  , insertGBinds
  , interPutError
  , interPutInfo
  , lookupCtx
  , lookupTerm
  , modifyInterEnv
  , outputCmdError
  , outputLibNotFoundError'
  , putInterEnv
  )

import Prelude hiding            (log)
import Control.Arrow             ((>>>))
import Control.Monad.IO.Class    (liftIO)
import Data.Bifunctor            (bimap)
import Data.List                 (intersperse)
import Language.KURE             (idR)
import Text.PrettyPrint.HughesPJ (renderStyle)

{-
  <TO-DO>: - Should ctx-eqs always have holes? We can force this easily.
             (add-lib and import-lib commands.)
            - Clean up spacing for show-lib - abstract it to InterPrintUtils.

  Information:
  -----------------------------------------------------------------------------
  - A number of commands for updating UNIE's library of
    terms/contexts/cost-eqs.
-}

-- Valid command names: -------------------------------------------------------

baseLibCmds :: [String]
baseLibCmds  = [ "add-lib"       -- Add definitions to the library from the command line.
               , "define"        -- Add definitions to the library from the active transformation.
               , "del-lib"       -- Delete definitions from the library, by name or index.
               , "export-lib"    -- Export definitions from the library to file.
               , "import-lib"    -- Import definitions to the library from file.
               , "show-lib" ]    -- Review definitions.

-- Matchers for above commands: -----------------------------------------------

matchers :: [Matcher]
matchers  =
  [ cmdMatcher "add-lib" RawBaseLibCmd
    [ [srcCodeMatcher], [cmdNameMatcher, srcCodeMatcher] ]

  , cmdMatcher "define" RawBaseLibCmd
    [ [srcNameMatcher], [srcCodeMatcher] ]

 , cmdMatcher "del-lib" RawBaseLibCmd
    [ [srcNameMatcher]
    , [cmdNameMatcher]
    , [cmdNameMatcher, numberMatcher] ]

 , cmdMatcher "export-lib" RawBaseLibCmd
    [ [fileMatcher]
    , [cmdNameMatcher, fileMatcher]
    , [cmdNameMatcher, cmdNameMatcher, fileMatcher] ]

 , cmdMatcher "import-lib" RawBaseLibCmd
    [ [fileMatcher]
    , [cmdNameMatcher, fileMatcher] ]

 , cmdMatcher "show-lib" RawBaseLibCmd
    [ []
    , [cmdNameMatcher]
    , [srcNameMatcher] ]
 ]

-- Refiner for above commands: ------------------------------------------------

refineRawBaseLibCmd :: Refiner

-- add-lib takes term/context source code or a cost-equiv. context
-- with its context kind specified explicitly.
refineRawBaseLibCmd (RawBaseLibCmd "add-lib" ps) =
  bimap ParamErr (BaseLibCmd "add-lib") $
     paramsRefine ps [ [ ctxKindRefine
                       , ctxPatSrcCodeRefine ]
                     , [tBindSrcCodeRefine]
                     , [cBindSrcCodeRefine] ]

-- define takes a term source name or a context variable (source code).
refineRawBaseLibCmd (RawBaseLibCmd "define" ps) =
  bimap ParamErr (BaseLibCmd "define") $
   paramsRefine ps [ [termSrcNameRefine], [cVarSrcCodeRefine] ]

-- del-lib's parameters detail what to delete:
-- All "terms", "ctxs", "ctx-eqs";
-- A term/ctx source name to delete a specific term/ctx.
-- A context kind and index to delete a specific cost-eq. context.
refineRawBaseLibCmd (RawBaseLibCmd "del-lib" ps) =
  bimap ParamErr (BaseLibCmd "del-lib") $
  paramsRefine ps [ [termSrcNameRefine]
                  , [ctxSrcNameRefine]
                  , [cmdNameRefine ["terms", "ctxs", "ctx-eqs"]]
                  , [ctxKindRefine]
                  , [ctxKindRefine, numberRefine] ]

-- import-lib with just a filepath imports terms and contexts from that file;
-- "terms"/"ctxs" can be used to import one type only;
-- A context kind and filepath imports the defs. from that file as cost-eq.
-- contexts of the specified kind.
refineRawBaseLibCmd (RawBaseLibCmd "import-lib" ps) =
  bimap ParamErr (BaseLibCmd "import-lib") $
    paramsRefine ps [ [fileRefine]
                    , [ cmdNameRefine ["terms", "ctxs"]
                      , fileRefine ]
                    , [ ctxKindRefine
                      , fileRefine ] ]

-- export-lib with just a filepath exports terms /and/ contexts to the file;
-- "terms"/"ctxs" can be used to export one type only;
-- A context kind and filepath exports all cost-equiv. contexts of that kind
-- to file.
refineRawBaseLibCmd (RawBaseLibCmd "export-lib" ps) =
  bimap ParamErr (BaseLibCmd "export-lib") $
     paramsRefine ps [ [fileRefine]
                     , [ cmdNameRefine ["terms", "ctxs"]
                       , fileRefine ]
                     , [ ctxKindRefine
                       , fileRefine ] ]

-- show-lib with no parameter shows the full library (note only
-- names for terms/ctxs are printed);
-- All "terms"/"ctxs" (names), "ctx-eqs";
-- Also have "defs" for terms/ctxs only.
-- A context kind to just display cost-equiv. contexts of that kind;
-- Terms/context definitions can be reviewed by name.
refineRawBaseLibCmd (RawBaseLibCmd "show-lib" ps)
 =  bimap ParamErr (BaseLibCmd "show-lib") $
     paramsRefine ps [ []
                     , [cmdNameRefine ["terms", "ctxs", "defs", "ctx-eqs"]]
                     , [ctxKindRefine]
                     , [termSrcNameRefine]
                     , [ctxSrcNameRefine] ]

-- Anything else is invalid.
refineRawBaseLibCmd cmd@RawBaseLibCmd{} =
  Left $ InternalErr $ UnexpectedCmd "refineRawBaseLibCmd" $ show cmd
refineRawBaseLibCmd _ = Left $ InternalErr $  WrongRefine "refineRawBaseLibCmd"


-- Interpreters for above commands, for all states: ---------------------------

baseLibInterp :: Interp

-- Add definitions to the library from command line: --------------------------

baseLibInterp (BaseLibCmd "add-lib" ps) _ _ = case ps of

  [TermSrcCode (UGBind tBind)] -> checkInsertTermsCtxs [tBind]
  [CtxSrcCode  (UGBind cBind)] -> checkInsertTermsCtxs [cBind]

  [CtxKind k, CtxPatSrcCode (UCtxPat pctx)] ->
  {-  -- The only basic check we do is to make sure the defn. has holes.
      -- The rest is up to the user.
    if hasHoles pctx
    then modifyInterEnv (insertCtxEqss [([pctx], k)]) >> updateMsg
    else interPutError "cost-equiv. contexts must have holes." -}

    -- I'm in two minds whether to let contexts without holes be added as
    -- ctx-eq. What would we use them for? Allowed for now.
    modifyInterEnv (insertCtxEqss [([pctx], k)]) >> updateMsg

  _ -> baseLibUnexpectedParams ps

-- Add definitions to the library from active transformation: -----------------
-- This is only available if a term is actually being transformed, i.e.,
-- TRANS/TRANS_SCRIPT state.

baseLibInterp cmd@(BaseLibCmd "define" ps) mrel st

    -- Need to be in a transformation state.
  | isTransState st = case ps of
      -- If we're defining a term, just get the current def. and save it,
      -- then return the defs. binder.
      [TermSrcName s] -> applyRTermCurrPathUpdate idR $
        \term -> checkTermsCtxs [TBind s term] >>= either interPutError
           (\gbinds -> modifyInterEnv (insertGBinds gbinds)
            >> updateMsg
            >> applyRTermCurrPathLog (cmd, mrel) (return $ Var s))

      -- If we're defining a context, need to generate one from the term
      -- by 'deleting' the substitution (replace with holes).
      [CtxSrcCode (UCtx (CVar k ns (Just sub)))] ->
        applyTTermCurrPathUpdate (idR >>> delCtxSubst sub) $
        -- Check the kind of the context defined to make sure it's correct.
        \ctx -> checkTermsCtxs [CBind k ns ctx] >>= either interPutError
           (\gbinds -> modifyInterEnv (insertGBinds gbinds)
             >> updateMsg
             >> applyRTermCurrPathLog (cmd, mrel) (return $ CVar k ns (Just sub)))

      -- Cannot define contexts by using CVars with empty substs. because it
      -- will never match the term being transformed.
      [CtxSrcCode (UCtx (CVar _ _ Nothing))] ->
        interPutError "Context variable must be substituted."

      -- Anything else is invalid.
      _ -> baseLibUnexpectedParams ps

    -- Can't define if not in a transformation mode.
  | otherwise = outputCmdError $ StateErr st

-- Delete definitions in the library: -----------------------------------------
-- Just a case of finding the appropriate def(s). to delete.

baseLibInterp (BaseLibCmd "del-lib" ps) _ _ = case ps of

  -- Delete a single term/ctx by name.
  [TermSrcName s] -> maybe (outputLibNotFoundError' s)
    (\env -> putInterEnv env >> updateMsg) =<< getInterEnv (deleteTerm s)
  [CtxSrcName s] -> maybe (outputLibNotFoundError' s)
    (\env -> putInterEnv env >> updateMsg) =<< getInterEnv (deleteCtx s)

  -- Delete all terms/ctxs/ctx-eqs.
  [CmdName "terms"]   -> modifyInterEnv deleteTerms >> updateMsg
  [CmdName "ctxs"]    -> modifyInterEnv deleteCtxs  >> updateMsg
  [CmdName "ctx-eqs"] -> modifyInterEnv InterUtils.deleteCtxEqss >> updateMsg

  -- Delete ctx-eqs. of a specific kind.
  [CtxKind k] -> modifyInterEnv (InterUtils.deleteCtxEqs k) >> updateMsg

  -- Delete a single ctx-eq of a specific kind by index.
  [CtxKind k, Number i] -> maybe (interPutError "invalid index.")
    (\env -> putInterEnv env >> updateMsg) =<< getInterEnv (InterUtils.deleteCtxEq k i)

  -- Anything else is invalid.
  _ -> baseLibUnexpectedParams ps

-- Export definitions fron the library: ---------------------------------------

baseLibInterp (BaseLibCmd "export-lib" ps) _ _ = case ps of

  -- Just a filepath exports terms and contexts.
  [File fp] -> baseLibExport fp (termsCtxsLibToFile True True)
  -- Terms/ctxs only.
  [CmdName "terms", File fp] -> baseLibExport fp (termsCtxsLibToFile True False)
  [CmdName "ctxs", File fp]  -> baseLibExport fp (termsCtxsLibToFile False True)
  -- Specific ctx-eq kind.
  [CtxKind k, File fp]       -> baseLibExport fp (ctxEqsToFile k)
  -- Anything else is invalid.
  _ -> baseLibUnexpectedParams ps

-- Import definitions into the library: ---------------------------------------

baseLibInterp (BaseLibCmd "import-lib" ps) _ _ = case ps of

  -- Import terms/contexts.
  -- Terms/ctxs if just a filepath, either can be specified as a param.
  [File fp]                  -> importTermsCtxs fp
  [CmdName "terms", File fp] -> importTermsCtxs fp
  [CmdName "ctxs", File fp]  -> importTermsCtxs fp

  -- Import cost-equiv. contexts.
  -- Here we're not enforcing them to have holes (yet).
  [CtxKind k, File fp] -> either interPutError
    ((>> updateMsg)
      . modifyInterEnv
      . insertCtxEqss
      . return
      . (, k)) =<< liftIO (parseFile' fp ctxs)

  -- Anything else is invalid.
  _ -> baseLibUnexpectedParams ps

-- Show definitions from the library: -----------------------------------------

-- I've added some manual spacing here for the time being. Will clean it
-- up when I upgrade the output next time.
baseLibInterp (BaseLibCmd "show-lib" ps) _ _ = case ps of

  -- All the library.
  [] -> do

    -- Project/format info. from library.
    ts  <- mapKeysToTerminal' "Term definitions library" <$>
            getInterEnv getTBinds
    cs  <- mapKeysToTerminal' "Context definitions library" <$>
            getInterEnv getCBinds
    eqs <- concat
            .  intersperse [" "]    -- Manually more add spacings for now.
            .  ctxEqLibToTerminal
           <$> getInterEnv getCtxEqs

    -- Display it using the brick library.
    liftIO . display . concat . intersperse [" "] $ [ts, cs, eqs] -- Manually spacing for now.

  -- Terms and contexts.
  [CmdName "defs"] -> do

    -- Project/format info. from library.
    ts  <- mapKeysToTerminal' "Term definitions library" <$>
            getInterEnv getTBinds
    cs  <- mapKeysToTerminal' "Context definitions library" <$>
            getInterEnv getCBinds

    -- Display it using the brick library.
    liftIO . display . concat . intersperse [" "] $ [ts, cs] -- Manually spacing for now.

  -- Terms/contexts names and cost-equivs only.
  [CmdName "terms"] -> displayLibNames "Term definitions library"
                        =<< getInterEnv getTBinds
  [CmdName "ctxs"]  -> displayLibNames "Context definitions library"
                        =<< getInterEnv getCBinds
  [CmdName "ctx-eqs"] -> do
    eqs <- concat
            .  intersperse [" "]    -- Manually more add spacings for now.
            .  ctxEqLibToTerminal
           <$> getInterEnv getCtxEqs

    liftIO . display $ eqs

  -- Term/context definitions.
  [TermSrcName s] -> do
   maybe (outputLibNotFoundError' s)
    (liftIO
      . display
      . return
      . renderStyle terminalLineStyle
      . ppr
      . TBind s) =<< getInterEnv (lookupTerm s)
  [CtxSrcName s] -> do
    maybe (outputLibNotFoundError' s)
     (\(k, ctx) -> liftIO
      . display
      . return
      . renderStyle terminalLineStyle
      . ppr
      $ CBind k s ctx) =<< getInterEnv (lookupCtx s)

  -- Cost-equivs. of a specific kind.
  [CtxKind k] -> liftIO
                  .  display
                  .  ctxEqsToTerminal k
                 =<< getInterEnv getCtxEqs

  -- Anything else is invalid.
  _ -> baseLibUnexpectedParams ps

 -- Error cases: --------------------------------------------------------------
baseLibInterp cmd@BaseLibCmd{} _ _ =
  outputCmdError $ InternalErr $ UnexpectedCmd "baseLibInter" $ show cmd
baseLibInterp _ _ _ = outputCmdError $ InternalErr $ WrongInter "baseLibInter"

-------------------------------------------------------------------------------
-- Helpers: --
-------------------------------------------------------------------------------

-- Error message for unexpected parameters.
baseLibUnexpectedParams :: [Param] -> InterM s ()
baseLibUnexpectedParams ps = outputCmdError
                              . InternalErr
                              . UnexpectedParams "baseLibInterp"
                              $ fmap show ps

-- Generic update message for when library has changed.
updateMsg :: InterM InterEnv ()
updateMsg  = interPutInfo "library updated."

-- Importing: --

-- Import terms/contexts from a filepath, they are both handled
-- using the gBinds parsers uniformly.
importTermsCtxs :: FilePath -> InterM InterEnv ()
importTermsCtxs fp = either interPutError checkInsertTermsCtxs
                      =<< liftIO (parseFile' fp gBinds)

-- Once parsed, we must check each context has a valid context kind.
checkInsertTermsCtxs :: [GBind] -> InterM InterEnv ()
checkInsertTermsCtxs gbinds =
  either interPutError (\gbinds -> modifyInterEnv
    (insertGBinds gbinds) >> updateMsg) =<< checkTermsCtxs gbinds

-- Does the actually checking of contexts, terms are just ignored.
checkTermsCtxs :: [GBind] -> InterM InterEnv (Either String [GBind])
checkTermsCtxs gbinds = do
  lib <- getInterEnv getCtxEqs
  case filter (cBindError lib) gbinds of
     []   -> return $ Right gbinds
     errs -> return
              . Left
              . ("invalid context kinds: " ++)
              . unwords
              . fmap (\(CBind _ ns _) -> ns)
              $ errs
  where
    -- Use the CtxCheck module here.
    cBindError lib (CBind k _ ctx) = not $ checkCtxKind lib ctx k
    cBindError _ _ = False
