{-# LANGUAGE LambdaCase #-}

module BasicKureCmd
  ( basicKureCmds -- Valid commands

  , interp_applyDef        -- Apply a definition.
  , interp_desugar         -- Desugar a term.
  , interp_genNewBinders   -- Generate fresh binders recursively.
  , interp_removeTick      -- Remove a tick.
  , interp_renameBinder    -- Rename a specific binder.
  , interp_sugar           -- Syntactically sugar a term.
  , interp_tickIntro       -- Add a tick.
  , interp_unapplyDef      -- Unapply a definition.

  -- Matchers/refiners for above commands: --

  , matcher_applyDef
  , matcher_desugar
  , matcher_genNewBinders
  , matcher_renameBinder
  , matcher_sugar
  , matcher_tickElim
  , matcher_tickIntro
  , matcher_unapplyDef
  , matcher_untickIntro
  , refiner_applyDef
  , refiner_desugar
  , refiner_genNewBinders
  , refiner_removeTick
  , refiner_renameBinder
  , refiner_sugar
  , refiner_tickIntro
  , refiner_unapplyDef


  ) where

import CmdAST        (Cmd(..), Param(..), RawCmd(..))
import CmdError      (CmdError(..), InternalError(..))
import InterState    (isTransState)
import CmdParser     (cmdMatcher, cmdMatcherNoParams, )
import ParamParser   (cmdNameMatcher, srcNameMatcher)
import ParamRefine   ( anyCmdNameRefine, cmdNameRefine
                     , ctxSrcNameRefine, paramsRefine
                     , termSrcNameRefine)
import Types         (Interp, Matcher, Refiner)

import BasicRewrites
  ( addTickR
  , capAvoidApplyCtxDefR
  , capAvoidApplyTermDefR
  , capAvoidUnapplyCtxDefR
  , capAvoidUnapplyTermDefR
  , nonCapAvoidApplyCtxDefR
  , nonCapAvoidApplyTermDefR
  , nonCapAvoidUnapplyCtxDefR
  , nonCapAvoidUnapplyTermDefR
  , removeTickR
  )

import InterUtils
  ( applyRTermCurrPathLog
  , applyRTermLog
  , getInterEnv
  , getPath
  , getTransEnv
  , lookupCtx
  , lookupTerm
  , outputCmdError
  , outputLibNotFoundError'
  )

import Normalisation
  ( normaliseAppDR
  , normaliseAppR
  , normaliseNonRecR
  , normaliseR
  , normaliseTickR
  , renameR
  , renameSingleR
  , sugarAppDR
  , sugarAppR'
  , sugarNonRecR
  , sugarR
  , sugarTickR)

import Data.Bifunctor (bimap)

{-
  Information:
  -----------------------------------------------------------------------------
  - KURE commands that constitute 'basic' rewrites on
    the term being transformed.
-}

-- All valid basic KURE commands: ---------------------------------------------

basicKureCmds :: [String]
basicKureCmds  = [ "apply-def"
                 , "unapply-def"
                 , "rename-binder"
                 , "gen-new-binders"
                 , "desugar"
                 , "sugar"
                 , "tick-intro"
                 , "tick-elim"
                 , "untick-intro"
                 ]

-------------------------------------------------------------------------------
-- apply-def: --
-------------------------------------------------------------------------------
-- Apply a term/context definition: capture avoiding or not capture avoiding.

matcher_applyDef :: Matcher
matcher_applyDef  = cmdMatcher "apply-def" RawKureCmd
  [[srcNameMatcher], [srcNameMatcher, cmdNameMatcher]]

refiner_applyDef :: Refiner
refiner_applyDef (RawKureCmd "apply-def" ps) =
  bimap ParamErr (KureCmd "apply-def") $ paramsRefine ps
   [ [termSrcNameRefine]
   , [ctxSrcNameRefine]
   , [termSrcNameRefine, cmdNameRefine ["nca"]] -- 'not capture avoiding'
   , [ctxSrcNameRefine, cmdNameRefine ["nca"]]
   ]
refiner_applyDef _ = Left $ InternalErr $ WrongRefine "refiner_applyDef"

interp_applyDef :: Interp
interp_applyDef cmd@(KureCmd "apply-def" ps) mrel st

    -- Only available in transformation states.
  | isTransState st = case ps of

      -- Applying a term's definition.
      [TermSrcName s] -> getInterEnv (lookupTerm s) >>= \case
         Nothing   -> outputLibNotFoundError' s
         Just term -> getTransEnv $ \transEnv -> applyRTermLog
          (cmd, mrel) (capAvoidApplyTermDefR s term $ getPath transEnv)

      -- Applying a term's definition: not capture avoiding.
      [TermSrcName s, CmdName "nca"] -> getInterEnv (lookupTerm s) >>= \case
         Nothing   -> outputLibNotFoundError' s
         Just term -> applyRTermCurrPathLog (cmd, mrel)
                       (nonCapAvoidApplyTermDefR s term)

       -- Applying a context's definition.
      [CtxSrcName s] -> getInterEnv (lookupCtx s) >>= \case
        Nothing -> outputLibNotFoundError' s
        Just (k, ctx) -> getTransEnv $ \transEnv -> applyRTermLog
         (cmd, mrel) (capAvoidApplyCtxDefR k s ctx $ getPath transEnv)

      -- Applying a context's definition: not capture avoiding.
      [CtxSrcName s, CmdName "nca"] -> getInterEnv (lookupCtx s) >>= \case
        Nothing -> outputLibNotFoundError' s
        Just (k, ctx) -> applyRTermCurrPathLog (cmd, mrel)
                       (nonCapAvoidApplyCtxDefR k s ctx)

      -- Anything else is invalid.
      _ -> outputCmdError $ InternalErr $ UnexpectedParams
            "interp_applyDef" $ fmap show ps

  -- State error if transformation not in progress.
  | otherwise = outputCmdError (StateErr st)

-- Error case.
interp_applyDef _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_applyDef"



-------------------------------------------------------------------------------
-- unapply-def: --
-------------------------------------------------------------------------------
-- Unapply a term/context definition, /capture avoiding/.

matcher_unapplyDef :: Matcher
matcher_unapplyDef  = cmdMatcher "unapply-def" RawKureCmd
  [[srcNameMatcher], [srcNameMatcher, cmdNameMatcher]]

refiner_unapplyDef :: Refiner
refiner_unapplyDef (RawKureCmd "unapply-def" ps) =
  bimap ParamErr (KureCmd "unapply-def") $ paramsRefine ps
   [ [termSrcNameRefine]
   , [ctxSrcNameRefine]
   , [termSrcNameRefine, cmdNameRefine ["nca"]] -- 'not capture avoiding'
   , [ctxSrcNameRefine, cmdNameRefine ["nca"]]
   ]
refiner_unapplyDef _ = Left $ InternalErr $ WrongRefine "refiner_unapplyDef"

interp_unapplyDef :: Interp
interp_unapplyDef cmd@(KureCmd "unapply-def" ps) mrel st

  -- Only available in transformation states.
  | isTransState st = case ps of

    -- Unapplying a term's definition.
    [TermSrcName s] -> getInterEnv (lookupTerm s) >>= \case
      Nothing -> outputLibNotFoundError' s
      Just term -> getTransEnv $ \transEnv -> applyRTermLog (cmd, mrel)
                    (capAvoidUnapplyTermDefR s term $ getPath transEnv)

     -- Unapplying a term's definition: not capture avoiding
    [TermSrcName s, CmdName "nca"] -> getInterEnv (lookupTerm s) >>= \case
      Nothing -> outputLibNotFoundError' s
      Just term -> applyRTermCurrPathLog (cmd, mrel)
                    (nonCapAvoidUnapplyTermDefR s term)

    -- Unapplying a context's definition.
    [CtxSrcName s]  -> getInterEnv (lookupCtx s) >>= \case
      Nothing -> outputLibNotFoundError' s
      Just (k, ctx) -> applyRTermCurrPathLog (cmd, mrel)
                        (capAvoidUnapplyCtxDefR k s ctx)

    -- Unapplying a context's definition: not capture avoiding.
    [CtxSrcName s, CmdName "nca"] -> getInterEnv (lookupCtx s) >>= \case
      Nothing -> outputLibNotFoundError' s
      Just (k, ctx) -> applyRTermCurrPathLog (cmd, mrel)
                        (nonCapAvoidUnapplyCtxDefR k s ctx)

    -- Anything else is invalid.
    _ -> outputCmdError $ InternalErr $ UnexpectedParams
           "interp_unapplyDef" $ fmap show ps

  -- State error if transformation not in progress.
  | otherwise = outputCmdError (StateErr st)

-- Error case.
interp_unapplyDef _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_unapplyDef"



-------------------------------------------------------------------------------
-- rename-binder: --
-------------------------------------------------------------------------------
-- Rename a single binder, can give hint for new binder.

matcher_renameBinder :: Matcher
matcher_renameBinder  =
  cmdMatcher "rename-binder" RawKureCmd
   [[cmdNameMatcher], [cmdNameMatcher, cmdNameMatcher]]

refiner_renameBinder :: Refiner
refiner_renameBinder (RawKureCmd "rename-binder" ps) =
  bimap ParamErr (KureCmd "rename-binder") $
   paramsRefine ps [[anyCmdNameRefine], [anyCmdNameRefine, anyCmdNameRefine]]
refiner_renameBinder _ = Left $ InternalErr $ WrongRefine "refiner_renameBinder"

interp_renameBinder :: Interp
interp_renameBinder cmd@(KureCmd "rename-binder" ps) mrel st

   -- Only valid in transformation state.
  | isTransState st = case ps of

     -- Binder to rename.
     [CmdName old] -> applyRTermCurrPathLog (cmd, mrel) (renameSingleR old Nothing)

     -- Binder to rename, plus new binder hint.
     [CmdName old, CmdName new] ->
      applyRTermCurrPathLog (cmd, mrel) (renameSingleR old $ Just new)

     -- Anything else is invalid.
     _ -> outputCmdError $ InternalErr $ UnexpectedParams
           "interp_renameBinder" $ fmap show ps

  -- Invalid if not a transformation state
  | otherwise = outputCmdError (StateErr st)

interp_renameBinder _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_renameBinder"



-------------------------------------------------------------------------------
-- gen-new-binders: --
-------------------------------------------------------------------------------

matcher_genNewBinders :: Matcher
matcher_genNewBinders  = cmdMatcherNoParams "gen-new-binders" RawKureCmd

refiner_genNewBinders :: Refiner
refiner_genNewBinders  (RawKureCmd "gen-new-binders" ps) =
  bimap ParamErr (KureCmd "gen-new-binders") $ paramsRefine ps [[]]
refiner_genNewBinders _ = Left $ InternalErr $ WrongRefine "refiner_genNewBinders"

interp_genNewBinders :: Interp
interp_genNewBinders cmd@(KureCmd "gen-new-binders" ps) mrel st

  -- Only in a transformation state.
  | isTransState st =  case ps of
      [] -> applyRTermCurrPathLog (cmd, mrel) renameR
      _  -> outputCmdError $ InternalErr $ UnexpectedParams
               "interp_genNewBinders" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)

interp_genNewBinders _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_genNewBinders"



-------------------------------------------------------------------------------
-- desugar: --
-------------------------------------------------------------------------------
-- Normalise various terms.

matcher_desugar :: Matcher
matcher_desugar  = cmdMatcher "desugar" RawKureCmd [[], [cmdNameMatcher]]

refiner_desugar :: Refiner
refiner_desugar (RawKureCmd "desugar" ps) =
  bimap ParamErr (KureCmd "desugar") $ paramsRefine ps
   [[], [cmdNameRefine ["app", "data", "nonrec", "tick"]]]
refiner_desugar _ = Left $ InternalErr $ WrongRefine "refiner_desugar"

interp_desugar :: Interp
interp_desugar cmd@(KureCmd "desugar" ps) mrel st
  -- Only in a transformation state.
  | isTransState st = case ps of
      -- No params = all desugaring options, recursively
      [] -> applyRTermCurrPathLog (cmd, mrel) normaliseR

      -- Individual options.
      [CmdName "app"]    -> applyRTermCurrPathLog (cmd, mrel) normaliseAppR
      [CmdName "data"]   -> applyRTermCurrPathLog (cmd, mrel) normaliseAppDR
      [CmdName "tick"]   -> applyRTermCurrPathLog (cmd, mrel) normaliseTickR

      -- All options, but top-level only
      [CmdName "nonrec"] -> applyRTermCurrPathLog (cmd, mrel) normaliseNonRecR

      -- Anything else is invalid.
      _  -> outputCmdError $ InternalErr $ UnexpectedParams
             "interp_desugar" $ fmap show ps

  -- Invalid state.
  | otherwise = outputCmdError (StateErr st)
interp_desugar _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_desugar"



-------------------------------------------------------------------------------
-- sugar: --
-------------------------------------------------------------------------------
-- Sugar various terms.

matcher_sugar :: Matcher
matcher_sugar  = cmdMatcher "sugar" RawKureCmd [[], [cmdNameMatcher]]

refiner_sugar :: Refiner
refiner_sugar (RawKureCmd "sugar" ps) =
  bimap ParamErr (KureCmd "sugar") $ paramsRefine ps
   [[], [cmdNameRefine ["app", "data", "nonrec", "tick"]]]
refiner_sugar _ = Left $ InternalErr $ WrongRefine "refiner_sugar"

interp_sugar :: Interp
interp_sugar cmd@(KureCmd "sugar" ps) mrel st
  -- Only in a transformation state.
  | isTransState st = case ps of
      -- No params = all desugaring options, recursively
      [] -> applyRTermCurrPathLog (cmd, mrel) sugarR

      -- Individual options.
      [CmdName "app"]    -> applyRTermCurrPathLog (cmd, mrel) sugarAppR'
      [CmdName "data"]   -> applyRTermCurrPathLog (cmd, mrel) sugarAppDR
      [CmdName "tick"]   -> applyRTermCurrPathLog (cmd, mrel) sugarTickR

      -- All options, but top-level only
      [CmdName "nonrec"] -> applyRTermCurrPathLog (cmd, mrel) sugarNonRecR

      -- Anything else is invalid.
      _ -> outputCmdError $ InternalErr $ UnexpectedParams
            "interp_sugar" $ fmap show ps

  -- Invalid state.
  | otherwise = outputCmdError (StateErr st)
interp_sugar _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_sugar"



-------------------------------------------------------------------------------
-- tick-intro: --
-------------------------------------------------------------------------------

matcher_tickIntro :: Matcher
matcher_tickIntro  = cmdMatcherNoParams "tick-intro" RawKureCmd

refiner_tickIntro :: Refiner
refiner_tickIntro (RawKureCmd "tick-intro" ps) =
  bimap ParamErr (KureCmd "tick-intro") $ paramsRefine ps [[]]
refiner_tickIntro _ = Left $ InternalErr $ WrongRefine "refiner_tickIntro"

interp_tickIntro :: Interp
interp_tickIntro cmd@(KureCmd "tick-intro" ps) mrel st
  | isTransState st = case ps of
      [] -> applyRTermCurrPathLog (cmd, mrel) addTickR
      _  -> outputCmdError $ InternalErr $ UnexpectedParams
              "interp_tickIntro" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
interp_tickIntro _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_tickIntro"



-------------------------------------------------------------------------------
-- tick-elim/untick-intro: --
-------------------------------------------------------------------------------

removeTickCmds :: [String]
removeTickCmds  = ["tick-elim", "untick-intro"]

matcher_tickElim :: Matcher
matcher_tickElim  = cmdMatcherNoParams "tick-elim" RawKureCmd

matcher_untickIntro :: Matcher
matcher_untickIntro  = cmdMatcherNoParams "untick-intro" RawKureCmd

refiner_removeTick :: Refiner
refiner_removeTick (RawKureCmd "tick-elim" ps) =
  bimap ParamErr (KureCmd "tick-elim") $ paramsRefine ps [[]]
refiner_removeTick (RawKureCmd "untick-intro" ps) =
  bimap ParamErr (KureCmd "untick-intro") $ paramsRefine ps [[]]
refiner_removeTick _ = Left $ InternalErr $ WrongRefine "refiner_removeTick"

interp_removeTick :: Interp
interp_removeTick cmd@(KureCmd s ps) mrel st
  | s `elem` removeTickCmds && isTransState st && null ps =
     applyRTermCurrPathLog (cmd, mrel) removeTickR
  | s `elem` removeTickCmds  && isTransState st =
      outputCmdError $ InternalErr $ UnexpectedParams
       "interp_removeTick" $ fmap show ps
  | s `elem` removeTickCmds = outputCmdError (StateErr st)
interp_removeTick _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_removeTick"
