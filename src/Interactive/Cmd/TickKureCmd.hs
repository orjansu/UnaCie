{-# LANGUAGE LambdaCase #-}

module TickKureCmd where

import CtxAST
import CtxKind
import CtxPatAST
import PPLib
import InterPrintUtils
import CmdAST
import CmdError
import InterEnv
import InterUtils -- applyRTermCurrPathLog among others.
import InterState (isTransState)
import CmdParser
import ParamParser
import ParamRefine
import Types
import Universes

-- Rewrites
import TickAlgebraRewrites

import Data.Bifunctor (bimap)
import Control.Monad.IO.Class
import Text.PrettyPrint.HughesPJ

{-
  <TO-DO>:

  Information:
  -----------------------------------------------------------------------------
  - KURE commands relating to the laws of M&S' Tick Algebra.

  Working notes:
  -----------------------------------------------------------------------------
  - We should ALWAYS log the context used, when one is generated on behalf
    of the user. This is because even if there is just one option generated
    in a particular instance, under different circumstances other context
    options may become available due to ctx-eqs.
-}

-------------------------------------------------------------------------------
-- beta: --
-------------------------------------------------------------------------------
-- Beta reduction. Doesn't require any parameters.

matcher_beta :: Matcher
matcher_beta  = cmdMatcherNoParams "beta" RawKureCmd

matcher_betaAll :: Matcher
matcher_betaAll  = cmdMatcherNoParams "beta-all" RawKureCmd

refiner_beta :: Refiner
refiner_beta (RawKureCmd s ps) | s `elem` ["beta", "beta-all"] =
  bimap ParamErr (KureCmd s) $ paramsRefine ps [[]]
refiner_beta _ = Left $ InternalErr $ WrongRefine "refiner_beta"

interp_beta :: Interp
interp_beta cmd@(KureCmd s ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case (s, ps) of
     ("beta", [])     -> applyRTermCurrPathLog (cmd, mrel) betaR
     ("beta", _)      -> err ps
     ("beta-all", []) -> applyRTermCurrPathLog (cmd, mrel) betaAllR
     ("beta-all", _)  -> err ps
     _ -> outputCmdError $ InternalErr $ WrongInter "interp_beta"
  | otherwise = outputCmdError (StateErr st)
  where err ps = outputCmdError $ InternalErr $ UnexpectedParams
                  "interp_beta" $ fmap show ps
-- Error case.
interp_beta _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_beta"



-------------------------------------------------------------------------------
-- unbeta: --
-------------------------------------------------------------------------------
-- Unapply beta reduction (i.e., beta expansion). Requires a compatible
-- function application parameter to replace the reduct.

matcher_unbeta :: Matcher
matcher_unbeta  = cmdMatcher "unbeta" RawKureCmd [[srcCodeMatcher]]

refiner_unbeta :: Refiner
refiner_unbeta (RawKureCmd "unbeta" ps) =
  bimap ParamErr (KureCmd "unbeta") $ paramsRefine ps [[funAppSrcCodeRefine]]
refiner_unbeta _ = Left $ InternalErr $ WrongRefine "refiner_unbeta"

interp_unbeta :: Interp
interp_unbeta cmd@(KureCmd "unbeta" ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [SrcCode (UCtx ctx)] -> applyRTermCurrPathLog (cmd, mrel) (unbetaR ctx)
      _ -> outputCmdError $ InternalErr $ UnexpectedParams
             "interp_unbeta" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
-- Error case.
interp_unbeta _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unbeta"



-------------------------------------------------------------------------------
-- case-beta: --
-------------------------------------------------------------------------------
-- Reduce a case statement, doesn't require any parameters.

matcher_caseBeta :: Matcher
matcher_caseBeta  = cmdMatcherNoParams "case-beta" RawKureCmd

refiner_caseBeta :: Refiner
refiner_caseBeta (RawKureCmd "case-beta" ps) =
  bimap ParamErr (KureCmd "case-beta") $ paramsRefine ps [[]]
refiner_caseBeta _ = Left $ InternalErr $ WrongRefine "refiner_caseBeta"

interp_caseBeta :: Interp
interp_caseBeta cmd@(KureCmd "case-beta" ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [] -> applyRTermCurrPathLog (cmd, mrel) caseBetaR
      _  -> outputCmdError $ InternalErr $ UnexpectedParams
              "interp_caseBeta" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
-- Error case.
interp_caseBeta _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_caseBeta"



-------------------------------------------------------------------------------
-- uncase-beta: --
-------------------------------------------------------------------------------
-- Unapply a case reduction, needs a compatible case statement parameter to
-- replace the 'case-reduct'.

matcher_uncaseBeta :: Matcher
matcher_uncaseBeta  = cmdMatcher "uncase-beta" RawKureCmd [[srcCodeMatcher]]

refiner_uncaseBeta :: Refiner
refiner_uncaseBeta (RawKureCmd "uncase-beta" ps) =
  bimap ParamErr (KureCmd "uncase-beta") $ paramsRefine ps [[caseSrcCodeRefine]]
refiner_uncaseBeta _ = Left $ InternalErr $ WrongRefine "refiner_uncaseBeta"

interp_uncaseBeta :: Interp
interp_uncaseBeta cmd@(KureCmd "uncase-beta" ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [SrcCode (UCtx ctx)] -> applyRTermCurrPathLog (cmd, mrel) (uncaseBetaR ctx)
      _ -> outputCmdError $ InternalErr $ UnexpectedParams
             "interp_uncaseBeta" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
-- Error case.
interp_uncaseBeta _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_uncaseBeta"



-------------------------------------------------------------------------------
-- gc: --
-------------------------------------------------------------------------------
-- Remove unused bindings.

matcher_gc :: Matcher
matcher_gc  = cmdMatcher "gc" RawKureCmd [[srcNameMatcher]]

matcher_gcAll :: Matcher
matcher_gcAll  = cmdMatcherNoParams "gc-all" RawKureCmd

refiner_gc :: Refiner
refiner_gc (RawKureCmd "gc" ps) =
  bimap ParamErr (KureCmd "gc") $ paramsRefine ps [[termSrcNameRefine]]
refiner_gc (RawKureCmd "gc-all" ps) =
  bimap ParamErr (KureCmd "gc-all") $ paramsRefine ps [[]]
refiner_gc _ = Left $ InternalErr $ WrongRefine "refiner_gc"

interp_gc :: Interp
-- A single unused binding.
interp_gc cmd@(KureCmd "gc" ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [TermSrcName s] -> applyRTermCurrPathLog (cmd, mrel) (gcR s)
      _ -> outputCmdError $ InternalErr $ UnexpectedParams
             "interp_gc" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
interp_gc cmd@(KureCmd "gc-all" ps) mrel st
-- All unused bindings.
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [] -> applyRTermCurrPathLog (cmd, mrel) gcAllR
      _ -> outputCmdError $ InternalErr $ UnexpectedParams
             "interp_gc" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
-- Error case.
interp_gc _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_gc"

-------------------------------------------------------------------------------
-- ungc: --
-------------------------------------------------------------------------------
-- Add unused bindings.

matcher_ungc :: Matcher
matcher_ungc  = cmdMatcher "ungc" RawKureCmd [[srcCodeMatcher]]

refiner_ungc :: Refiner
refiner_ungc (RawKureCmd "ungc" ps) =
  bimap ParamErr (KureCmd "ungc") $ paramsRefine ps [[bindSrcCodeRefine]]
refiner_ungc _ = Left $ InternalErr $ WrongRefine "refiner_ungc"

interp_ungc :: Interp
-- A single unused binding.
interp_ungc cmd@(KureCmd "ungc" ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [TermSrcCode (UBind b)] -> applyRTermCurrPathLog (cmd, mrel) (ungcR [b])
      _ -> outputCmdError $ InternalErr $ UnexpectedParams "interp_ungc" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
-- Error case.
interp_ungc _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_ungc"


-------------------------------------------------------------------------------
-- let-flatten: --
-------------------------------------------------------------------------------
-- 'Flatten' nested let statements into one.

matcher_letFlatten :: Matcher
matcher_letFlatten  = cmdMatcherNoParams "let-flatten" RawKureCmd

refiner_letFlatten :: Refiner
refiner_letFlatten (RawKureCmd "let-flatten" ps) =
  bimap ParamErr (KureCmd "let-flatten") $ paramsRefine ps [[]]
refiner_letFlatten _ = Left $ InternalErr $ WrongRefine "refiner_letFlatten"

interp_letFlatten :: Interp
interp_letFlatten cmd@(KureCmd "let-flatten" ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [] -> applyRTermCurrPathLog (cmd, mrel) letFlattenR
      _ -> outputCmdError $ InternalErr $ UnexpectedParams "interp_letFlatten" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
-- Error case.
interp_letFlatten _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_letFlatten"

-------------------------------------------------------------------------------
-- unlet-flatten: --
-------------------------------------------------------------------------------
-- 'Unflatten' nested let statements by splitting.
-- ATM can only split one binding off at a time.

matcher_unletFlatten :: Matcher
matcher_unletFlatten  = cmdMatcher "unlet-flatten" RawKureCmd [[srcNameMatcher]]

refiner_unletFlatten :: Refiner
refiner_unletFlatten (RawKureCmd "unlet-flatten" ps) =
  bimap ParamErr (KureCmd "unlet-flatten") $ paramsRefine ps [[termSrcNameRefine]]
refiner_unletFlatten _ = Left $ InternalErr $ WrongRefine "refiner_unletFlatten"

interp_unletFlatten :: Interp
interp_unletFlatten cmd@(KureCmd "unlet-flatten" ps) mrel st
  -- Only available in a transformation state.
  | isTransState st = case ps of
      [TermSrcName s] -> applyRTermCurrPathLog (cmd, mrel) (unletFlattenR [s])
      _ -> outputCmdError $ InternalErr $ UnexpectedParams "interp_unletFlatten" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
-- Error case.
interp_unletFlatten _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unletFlatten"






-------------------------------------------------------------------------------
-- value-beta: --
-------------------------------------------------------------------------------
-- Applies the tick algebra law value-beta, requires one or two parameters:
-- (mandatory) name of the binding to use with value-beta
-- (optional)  context option to use with the law (might be a pattern).

matcher_valueBeta :: Matcher
matcher_valueBeta  = cmdMatcher "value-beta" RawKureCmd
  [[srcNameMatcher], [srcNameMatcher, srcCodeMatcher]]

refiner_valueBeta :: Refiner
refiner_valueBeta (RawKureCmd "value-beta" ps) =
  bimap ParamErr (KureCmd "value-beta") $ paramsRefine ps
    [ [termSrcNameRefine]                        -- Just source name
    , [termSrcNameRefine, ctxPatSrcCodeRefine]   -- Source name + context pattern
    , [termSrcNameRefine, ctxSrcCodeRefine] ]    -- Source name + context
refiner_valueBeta _ = Left $ InternalErr $ WrongRefine "refiner_valueBeta"

interp_valueBeta :: Interp
interp_valueBeta cmd@(KureCmd "value-beta" ps) mrel st

  -- The command is only available in a transformation state.
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [TermSrcName ns] -> do

       -- Check to see if term is in library, in case user
       -- is referring to that.
       mterm <- getInterEnv (lookupTerm ns)
       -- Get ctx-eqs to be used for context generation.
       lib   <- getInterEnv getCtxEqs

       -- Use the valueBeta /generate/ transformation to provide
       -- context options for the user
       applyTTermCurrPathUpdate (valueBetaGenT ns lib mterm) $ \case

         -- If no options are returned, then the rule /cannot/ be applied.
         [] -> outputCmdError (KureErr "no valid standard contexts.")

         -- If just one option is returned, then execute the
         -- command straight away. We log which context the rule has been used
         -- in conjunction with.
         [ctx] -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (valueBetaR ns ctx lib mterm)
         ctxs  -> userChoice ns lib mterm ctxs

     -- Match the context option from a specific context pattern: -------------

     [TermSrcName ns, CtxPatSrcCode (UCtxPat pctx)] -> do

       -- As above, but now use the valueBeta /match/ transformation to provide
       -- context options for the user
       mterm <- getInterEnv (lookupTerm ns)
       lib   <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (valueBetaMatchT ns lib mterm pctx) $ \case

        -- If no options are returned, then the specified pattern is incompatible
        -- with the valueBeta
        [] -> outputCmdError (KureErr "invalid context pattern.")

        -- As above.
        [ctx] -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (valueBetaR ns ctx lib mterm)
        ctxs  -> userChoice ns lib mterm ctxs

     -- Use the given context: --

     [TermSrcName ns, CtxSrcCode (UCtx ctx)] -> do
       -- This time we just apply valueBetaR straight away.
       mterm <- getInterEnv (lookupTerm ns)
       lib   <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (valueBetaR ns ctx lib mterm)


     -- Any other params. are invalid: ----------------------------------------
     _ -> outputCmdError $ InternalErr $ UnexpectedParams
           "interp_valueBeta" $ fmap show ps

    -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)


  where
    -- Generate the full command to be logged in UNIE's history.
    genCmd ns ctx = KureCmd "value-beta" [TermSrcName ns, CtxSrcCode (UCtx ctx)]

    -- Ask user to select a context option, or cancel.
    userChoice ns lib mterm ctxs = ctxChoices ctxs "Select a context option:" >>= \case
      Nothing  -> interPutInfo "cancelled."
      Just ctx -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (valueBetaR ns ctx lib mterm)

-- Error case.
interp_valueBeta _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_valueBeta"



-------------------------------------------------------------------------------
-- unvalue-beta: --
-------------------------------------------------------------------------------
-- Applies the tick algebra law value-beta from right to left, requires one
-- or two parameters:
-- (mandatory) name of the binding to use with unvalue-beta
-- (optional)  context option to use with the law (might be a pattern).

matcher_unvalueBeta :: Matcher
matcher_unvalueBeta  = cmdMatcher "unvalue-beta" RawKureCmd
     [[srcNameMatcher], [srcNameMatcher, srcCodeMatcher]]

refiner_unvalueBeta :: Refiner
refiner_unvalueBeta (RawKureCmd "unvalue-beta" ps) =
  bimap ParamErr (KureCmd "unvalue-beta") $
     paramsRefine ps [ [termSrcNameRefine]
                     , [termSrcNameRefine, ctxPatSrcCodeRefine]
                     , [termSrcNameRefine, ctxSrcCodeRefine]
                     ]
refiner_unvalueBeta _ = Left $ InternalErr $ WrongRefine "refiner_unvalueBeta"

interp_unvalueBeta :: Interp
interp_unvalueBeta cmd@(KureCmd "unvalue-beta" ps) mrel st
  | isTransState st = case ps of

    -- Generate context options on behalf of the user: ------------------------

     [TermSrcName ns] -> do
       mterm <- getInterEnv (lookupTerm ns)
       lib   <- getInterEnv getCtxEqs

       applyTTermCurrPathUpdate (unvalueBetaGenT ns lib mterm) $ \case

         -- No valid contexts generated by the system.
         [] -> outputCmdError (KureErr "no valid standard contexts.")

         -- Single option, so execute the rule and log precise command.
         [ctx] -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (unvalueBetaR ns ctx lib mterm)

         -- Multiple options, ask user to decide.
         ctxs -> userChoice ns lib mterm ctxs

     -- Match the context option from a specific context pattern: -------------

     [TermSrcName ns, CtxPatSrcCode (UCtxPat pctx)] -> do
       mterm <- getInterEnv (lookupTerm ns)
       lib   <- getInterEnv getCtxEqs

       applyTTermCurrPathUpdate (unvalueBetaMatchT ns lib mterm pctx) $ \case
        [] -> outputCmdError (KureErr "invalid context pattern.")
        [ctx] -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (unvalueBetaR ns ctx lib mterm)
        ctxs -> userChoice ns lib mterm ctxs

     -- Use the given context: ------------------------------------------------

     [TermSrcName ns, CtxSrcCode (UCtx ctx)] -> do
        mterm <- getInterEnv (lookupTerm ns)
        lib   <- getInterEnv getCtxEqs
        applyRTermCurrPathLog (cmd, mrel) (unvalueBetaR ns ctx lib mterm)

     -- Any other params. are invalid: ----------------------------------------
     _ -> outputCmdError $ InternalErr $ UnexpectedParams
           "interp_unvalueBeta" $ fmap show ps

    -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

 where
   -- Generate the full command to be logged in UNIE's history.
   genCmd ns ctx = KureCmd "unvalue-beta" [TermSrcName ns, CtxSrcCode (UCtx ctx)]

   -- Ask user to select a context option, or cancel.
   userChoice ns lib mterm ctxs = ctxChoices ctxs "Select a context option:" >>= \case
     Nothing  -> interPutInfo "cancelled."
     Just ctx -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (unvalueBetaR ns ctx lib mterm)

-- Error case.
interp_unvalueBeta _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unvalueBeta"


-------------------------------------------------------------------------------
-- var-beta: --
-------------------------------------------------------------------------------

matcher_varBeta :: Matcher
matcher_varBeta  = cmdMatcher "var-beta" RawKureCmd
  [[srcNameMatcher], [srcNameMatcher, srcCodeMatcher]]

matcher_varBetaWCE :: Matcher
matcher_varBetaWCE = cmdMatcher "var-beta-wce" RawKureCmd
  [[srcNameMatcher], [srcNameMatcher, srcCodeMatcher]]

refiner_varBeta :: Refiner
refiner_varBeta (RawKureCmd s ps) | s `elem` ["var-beta", "var-beta-wce"] =
  bimap ParamErr (KureCmd s) $ paramsRefine ps
    [ [termSrcNameRefine]                        -- Just source name
    , [termSrcNameRefine, ctxPatSrcCodeRefine]   -- Source name + context pattern
    , [termSrcNameRefine, ctxSrcCodeRefine] ]    -- Source name + context
refiner_varBeta _ = Left $ InternalErr $ WrongRefine "refiner_varBeta"

interp_varBeta :: Interp
interp_varBeta cmd@(KureCmd s ps) mrel st

  -- The command is only available in a transformation state.
  | isTransState st && s `elem` ["var-beta", "var-beta-wce"] = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [TermSrcName ns] -> do

       -- Check to see if term is in library, in case user
       -- is referring to that.
       mterm <- getInterEnv (lookupTerm ns)
       -- Get ctx-eqs to be used for context generation.
       lib   <- getInterEnv getCtxEqs

       -- Use the valueBeta /generate/ transformation to provide
       -- context options for the user
       applyTTermCurrPathUpdate (varBetaGenT ns lib mterm) $ \case

         -- If no options are returned, then the rule /cannot/ be applied.
         [] -> outputCmdError (KureErr "no valid standard contexts.")

         -- If just one option is returned, then execute the
         -- command straight away. We log which context the rule has been used
         -- in conjunction with.
         [ctx] -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (varBetaR ns ctx lib mterm)
         ctxs  -> userChoice ns lib mterm ctxs

     -- Match the context option from a specific context pattern: -------------

     [TermSrcName ns, CtxPatSrcCode (UCtxPat pctx)] -> do

       -- As above, but now use the valueBeta /match/ transformation to provide
       -- context options for the user
       mterm <- getInterEnv (lookupTerm ns)
       lib   <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (varBetaMatchT ns lib mterm pctx) $ \case

        -- If no options are returned, then the specified pattern is incompatible
        -- with the valueBeta
        [] -> outputCmdError (KureErr "invalid context pattern.")

        -- As above.
        [ctx] -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (varBetaR ns ctx lib mterm)
        ctxs  -> userChoice ns lib mterm ctxs

     -- Use the given context: --

     [TermSrcName ns, CtxSrcCode (UCtx ctx)] -> do
       -- This time we just apply valueBetaR straight away.
       mterm <- getInterEnv (lookupTerm ns)
       lib   <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (valueBetaR ns ctx lib mterm)


     -- Any other params. are invalid: ----------------------------------------
     _ -> outputCmdError $ InternalErr $ UnexpectedParams
           "interp_varBeta" $ fmap show ps


    -- Invalid state as not transformation.
  | isTransState st = outputCmdError $ InternalErr $ UnexpectedParams "interp_varBeta" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)


  where
    -- Generate the full command to be logged in UNIE's history.
    genCmd ns ctx = KureCmd "var-beta" [TermSrcName ns, CtxSrcCode (UCtx ctx)]

    -- Ask user to select a context option, or cancel.
    userChoice ns lib mterm ctxs = ctxChoices ctxs "Select a context option:" >>= \case
      Nothing  -> interPutInfo "cancelled."
      Just ctx -> applyRTermCurrPathLog (genCmd ns ctx, mrel) (varBetaR ns ctx lib mterm)

-- Error case.
interp_varBeta _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_varBeta"

-------------------------------------------------------------------------------
-- var-subst: --
-------------------------------------------------------------------------------
matcher_varSubst :: Matcher
matcher_varSubst _ = [] --[Right $ RawKureCmd "let-float-val" []]

refiner_varSubst :: Refiner
refiner_varSubst _ = Left $ InternalErr $ WrongRefine "refiner error varSubst"

interp_varSubst :: Interp
interp_varSubst _ _ _ = outputCmdError $ InternalErr $ WrongInter "interpreter error varSubst"

-------------------------------------------------------------------------------
-- let-float-val: --
-------------------------------------------------------------------------------

matcher_letFloatVal :: Matcher
matcher_letFloatVal  = cmdMatcher "let-float-val" RawKureCmd [[], [srcCodeMatcher]]

refiner_letFloatVal :: Refiner
refiner_letFloatVal (RawKureCmd "let-float-val" ps) =
  bimap ParamErr (KureCmd "let-float-val") $ paramsRefine ps
   [[], [ctxPatSrcCodeRefine], [ctxSrcCodeRefine]]
refiner_letFloatVal _ = Left $ InternalErr $ WrongRefine "refiner_letFloatVal"

interp_letFloatVal :: Interp
interp_letFloatVal cmd@(KureCmd "let-float-val" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
        lib <- getInterEnv getCtxEqs
        applyTTermCurrPathUpdate (letFloatValGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid standard contexts.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (letFloatValR lib ctx)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (letFloatValMatchT lib pctx) $ \case
         [] -> outputCmdError (KureErr "invalid context pattern.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (letFloatValR lib ctx)
         ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (letFloatValR lib ctx)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_letFloatVal" $ fmap show ps

   -- Invalid state as not transformation.
 | otherwise = outputCmdError (StateErr st)

   where
     -- Generate the full command to be logged in UNIE's history.
     genCmd ctx = KureCmd "let-float-val" [CtxSrcCode (UCtx ctx)]

     -- Ask user to select a context option, or cancel.
     userChoice ps lib = ctxSubChoices ps STD "Select a context/substitution option:" >>= \case
       Nothing -> interPutInfo "cancelled."
       Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (letFloatValR lib ctx)

-- Error case.
interp_letFloatVal _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_letFloatVal"


-------------------------------------------------------------------------------
-- unlet-float-val: --
-------------------------------------------------------------------------------

matcher_unletFloatVal :: Matcher
matcher_unletFloatVal  = cmdMatcher "unlet-float-val" RawKureCmd [[], [srcCodeMatcher]]

refiner_unletFloatVal :: Refiner
refiner_unletFloatVal (RawKureCmd "unlet-float-val" ps) =
  bimap ParamErr (KureCmd "unlet-float-val") $
   paramsRefine ps [[], [ctxPatSrcCodeRefine], [ctxSrcCodeRefine]]
refiner_unletFloatVal _ = Left $ InternalErr $ WrongRefine "refiner_unletFloatVal"

interp_unletFloatVal :: Interp
interp_unletFloatVal cmd@(KureCmd "unlet-float-val" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (unletFloatValGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid standard contexts.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (unletFloatValR lib ctx)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (unletFloatValMatchT lib pctx) $ \case
         [] -> outputCmdError (KureErr "invalid context pattern.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (unletFloatValR lib ctx)
         ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (unletFloatValR lib ctx)

     -- Any other params. are invalid: ----------------------------------------
     _ -> outputCmdError $ InternalErr $ UnexpectedParams "interp_unletFloatVal" $ fmap show ps

 -- Invalid state as not transformation.
 | otherwise = outputCmdError (StateErr st)

 where
   -- Generate the full command to be logged in UNIE's history.
   genCmd ctx = KureCmd "unlet-float-val" [CtxSrcCode (UCtx ctx)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices ps STD "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (unletFloatValR lib ctx)

-- Error case.
interp_unletFloatVal _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unletFloatVal"


-------------------------------------------------------------------------------
-- tick-eval: --
-------------------------------------------------------------------------------

matcher_tickEval :: Matcher
matcher_tickEval  = cmdMatcher "tick-eval" RawKureCmd [[], [srcCodeMatcher]]

refiner_tickEval :: Refiner
refiner_tickEval (RawKureCmd "tick-eval" ps) =
  bimap ParamErr (KureCmd "tick-eval") $ paramsRefine ps [[], [ctxSrcCodeRefine]]
refiner_tickEval _ = Left $ InternalErr $ WrongRefine "refiner_tickEval"

interp_tickEval :: Interp
interp_tickEval cmd@(KureCmd "tick-eval" ps) mrel st
  | isTransState st = case ps of

      -- Generate context options on behalf of the user: ----------------------

      [] -> do
        lib <- getInterEnv getCtxEqs
        applyTTermCurrPathUpdate (tickEvalGenT lib) $ \case
          [] -> outputCmdError (KureErr "no valid evaluation contexts.")
          [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (tickEvalR lib ctx)
          ps -> userChoice ps lib

      -- Match the context option from a specific context pattern: ------------

      [CtxPatSrcCode (UCtxPat pctx)] -> do
        lib <- getInterEnv getCtxEqs
        applyTTermCurrPathUpdate (tickEvalMatchT lib pctx) $ \case
         [] -> outputCmdError (KureErr "invalid context pattern.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (tickEvalR lib ctx)
         ps -> userChoice ps lib

       -- Use the given context: ----------------------------------------------

      [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (tickEvalR lib ctx)

      -- Any other params. are invalid: ---------------------------------------
      _ -> outputCmdError $ InternalErr $ UnexpectedParams  "interp_tickEval" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

 where
   -- Generate the full command to be logged in UNIE's history.
   genCmd ctx = KureCmd "tick-eval" [CtxSrcCode (UCtx ctx)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices ps EVAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (tickEvalR lib ctx)

-- Error case.
interp_tickEval _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_tickEval"



-------------------------------------------------------------------------------
-- untick-eval: --
-------------------------------------------------------------------------------

matcher_untickEval :: Matcher
matcher_untickEval  = cmdMatcher "untick-eval" RawKureCmd [[], [srcCodeMatcher]]

refiner_untickEval :: Refiner
refiner_untickEval (RawKureCmd "untick-eval" ps) =
  bimap ParamErr (KureCmd "untick-eval") $ paramsRefine ps [[], [ctxPatSrcCodeRefine]]
refiner_untickEval _ = Left $ InternalErr $ WrongRefine "refiner_untickEval"

interp_untickEval :: Interp
interp_untickEval cmd@(KureCmd "untick-eval" ps) mrel st
  | isTransState st = case ps of

      -- Generate context options on behalf of the user: ----------------------

      [] -> do
        lib <- getInterEnv getCtxEqs
        applyTTermCurrPathUpdate (untickEvalGenT lib) $ \case
          [] -> outputCmdError (KureErr "no valid evaluation contexts.")
          [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (untickEvalR lib ctx)
          ps -> userChoice ps lib

      -- Match the context option from a specific context pattern: ------------

      [CtxPatSrcCode (UCtxPat pctx)] -> do
        lib <- getInterEnv getCtxEqs
        applyTTermCurrPathUpdate (untickEvalMatchT lib pctx) $ \case
         [] -> outputCmdError (KureErr "invalid context pattern.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (untickEvalR lib ctx)
         ps -> userChoice ps lib

       -- Use the given context: ----------------------------------------------

      [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (untickEvalR lib ctx)

      -- Any other params. are invalid: ---------------------------------------
      _ -> outputCmdError $ InternalErr $ UnexpectedParams  "interp_untickEval" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

 where
   -- Generate the full command to be logged in UNIE's history.
   genCmd ctx = KureCmd "untick-eval" [CtxSrcCode (UCtx ctx)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices ps EVAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (untickEvalR lib ctx)

interp_untickEval _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_untickEval"



-------------------------------------------------------------------------------
-- let-eval: --
-------------------------------------------------------------------------------

matcher_letEval :: Matcher
matcher_letEval  = cmdMatcher "let-eval" RawKureCmd [[], [srcCodeMatcher]]

refiner_letEval :: Refiner
refiner_letEval (RawKureCmd "let-eval" ps) =
  bimap ParamErr (KureCmd "let-eval") $ paramsRefine ps
   [[], [ctxPatSrcCodeRefine]]
refiner_letEval _ = Left $ InternalErr $ WrongRefine "refiner_letEval"

interp_letEval :: Interp
interp_letEval cmd@(KureCmd "let-eval" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (letEvalGenT lib) $ \case
         [] -> outputCmdError (KureErr "nothing to substitute.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (letEvalR lib ctx)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (letEvalMatchT lib pctx) $ \case
         [] -> outputCmdError (KureErr "invalid context pattern.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (letEvalR lib ctx)
         ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (letEvalR lib ctx)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_letEval" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

  where
    -- Generate the full command to be logged in UNIE's history.
    genCmd ctx = KureCmd "let-eval" [CtxSrcCode (UCtx ctx)]

    -- Ask user to select a context option, or cancel.
    userChoice ps lib = ctxSubChoices ps EVAL "Select a context/substitution option:" >>= \case
      Nothing -> interPutInfo "cancelled."
      Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (letEvalR lib ctx)

-- Error case.
interp_letEval _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_letEval"



-------------------------------------------------------------------------------
-- unlet-eval: --
-------------------------------------------------------------------------------

matcher_unletEval :: Matcher
matcher_unletEval  = cmdMatcher "unlet-eval" RawKureCmd [[], [srcCodeMatcher]]

refiner_unletEval :: Refiner
refiner_unletEval (RawKureCmd "unlet-eval" ps) =
  bimap ParamErr (KureCmd "unlet-eval") $ paramsRefine ps [[], [ctxPatSrcCodeRefine]]
refiner_unletEval _ = Left $ InternalErr $ WrongRefine "refiner_unletEval"

interp_unletEval :: Interp
interp_unletEval cmd@(KureCmd "unlet-eval" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (unletEvalGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid evaluation contexts.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (unletEvalR lib ctx)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (unletEvalMatchT lib pctx) $ \case
         [] -> outputCmdError (KureErr "invalid context pattern.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (unletEvalR lib ctx)
         ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (unletEvalR lib ctx)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_unletEval" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

 where
    -- Generate the full command to be logged in UNIE's history.
    genCmd ctx = KureCmd "unlet-eval" [CtxSrcCode (UCtx ctx)]

    -- Ask user to select a context option, or cancel.
    userChoice ps lib = ctxSubChoices ps EVAL "Select a context/substitution option:" >>= \case
      Nothing -> interPutInfo "cancelled."
      Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (unletEvalR lib ctx)

-- Error case.
interp_unletEval _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unletEval"



-------------------------------------------------------------------------------
-- case-eval: --
-------------------------------------------------------------------------------

matcher_caseEval :: Matcher
matcher_caseEval  = cmdMatcher "case-eval" RawKureCmd [[], [srcCodeMatcher]]

refiner_caseEval :: Refiner
refiner_caseEval (RawKureCmd "case-eval" ps) =
  bimap ParamErr (KureCmd "case-eval") $ paramsRefine ps [[], [ctxPatSrcCodeRefine]]
refiner_caseEval _ = Left $ InternalErr $ WrongRefine "refiner_caseEval"

interp_caseEval :: Interp
interp_caseEval cmd@(KureCmd "case-eval" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (caseEvalGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid evaluation contexts")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (caseEvalR lib ctx)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (caseEvalMatchT lib pctx) $ \case
         [] -> outputCmdError (KureErr "invalid context pattern.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (caseEvalR lib ctx)
         ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (caseEvalR lib ctx)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_caseEval" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

 where
   -- Generate the full command to be logged in UNIE's history.
   genCmd ctx = KureCmd "case-eval" [CtxSrcCode (UCtx ctx)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices ps EVAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (caseEvalR lib ctx)

-- Error case.
interp_caseEval _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_caseEval"



-------------------------------------------------------------------------------
-- uncase-eval: --
-------------------------------------------------------------------------------

matcher_uncaseEval :: Matcher
matcher_uncaseEval  = cmdMatcher "uncase-eval" RawKureCmd  [[], [srcCodeMatcher]]

refiner_uncaseEval :: Refiner
refiner_uncaseEval (RawKureCmd "uncase-eval" ps) =
  bimap ParamErr (KureCmd "uncase-eval") $ paramsRefine ps
   [[], [ctxPatSrcCodeRefine], [ctxSrcCodeRefine]]
refiner_uncaseEval _ = Left $ InternalErr $ WrongRefine "refiner_uncaseEval"

interp_uncaseEval :: Interp
interp_uncaseEval cmd@(KureCmd "uncase-eval" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (uncaseEvalGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid evaluation context.")
         [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (uncaseEvalR lib ctx)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
      lib <- getInterEnv getCtxEqs
      applyTTermCurrPathUpdate (uncaseEvalMatchT lib pctx) $ \case
       [] -> outputCmdError (KureErr "invalid context pattern.")
       [(ctx, _)] -> applyRTermCurrPathLog (genCmd ctx, mrel) (uncaseEvalR lib ctx)
       ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx ctx)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (uncaseEvalR lib ctx)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_uncaseEval" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

  where
    -- Generate the full command to be logged in UNIE's history.
   genCmd ctx = KureCmd "uncase-eval" [CtxSrcCode (UCtx ctx)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices' ps EVAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (ctx, _) -> applyRTermCurrPathLog (genCmd ctx, mrel) (uncaseEvalR lib ctx)

-- Error case.
interp_uncaseEval _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_uncaseEval"

-------------------------------------------------------------------------------
-- eval-i: --
-------------------------------------------------------------------------------

matcher_eval_I ::  Matcher
matcher_eval_I  =  cmdMatcher "eval-i" RawKureCmd [[]]

refiner_eval_I :: Refiner
refiner_eval_I (RawKureCmd "eval-i" ps)
 =  bimap ParamErr (KureCmd "eval-i") $ paramsRefine ps [[]]
refiner_eval_I _ = Left $ InternalErr $ WrongRefine "refiner_eval_I"

interp_eval_I :: Interp
interp_eval_I cmd@(KureCmd "eval-i" ps) mrel st
  | isTransState st = go
  | otherwise       = outputCmdError (StateErr st)
 where go = case ps of
             [] -> do
              tBinds <- getInterEnv getTBinds
              applyRTermCurrPathLog (cmd, mrel) (evalInContextR tBinds)
             _  -> outputCmdError $ InternalErr $ UnexpectedParams
                    "interp_eval_I" $ fmap show ps
interp_eval_I _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_eval_I"


-------------------------------------------------------------------------------
-- eval-wce: --
-------------------------------------------------------------------------------

matcher_eval_WCE ::  Matcher
matcher_eval_WCE  =  cmdMatcher "eval-wce" RawKureCmd [[]]

refiner_eval_WCE :: Refiner
refiner_eval_WCE (RawKureCmd "eval-wce" ps)
 =  bimap ParamErr (KureCmd "eval-wce") $ paramsRefine ps [[]]
refiner_eval_WCE _ = Left $ InternalErr $ WrongRefine "refiner_eval_WCE"

interp_eval_WCE :: Interp
interp_eval_WCE cmd@(KureCmd "eval-wce" ps) mrel st
  | isTransState st = go
  | otherwise       = outputCmdError (StateErr st)
 where go = case ps of
             [] -> do
              tBinds <- getInterEnv getTBinds
              applyRTermCurrPathLog (cmd, mrel) (evalInContextR tBinds)
             _  -> outputCmdError $ InternalErr $ UnexpectedParams
                    "interp_eval_WCE" $ fmap show ps
interp_eval_WCE _ _ _ = outputCmdError $ InternalErr $
                         WrongInter "interp_eval_WCE"

-- Helpers: -------------------------------------------------------------------

ctxChoices :: [Ctx] -> String -> InterM InterEnv (Maybe Ctx)
ctxChoices [] _ = return Nothing
ctxChoices ctxs prompt
 =  do
      liftIO $ mapM_ putStrLn $ terminalMultiNumberedList prompt outputs
      fmap (choices !!) <$> usrChoiceIdxWithCancel maxIdx
    where
         choices = ctxs
         outputs = fmap (lines . renderStyle (genStyle
                    terminalInnerFramedWidth) . ppr) choices
         maxIdx  = length choices - 1


ctxSubChoices :: [(Ctx, Term)]
                 -> CtxKind
                 -> String
                 -> InterM InterEnv (Maybe (Ctx, Term))
ctxSubChoices [] _ _ = return Nothing
ctxSubChoices ps k prompt
 =  do
      liftIO $ mapM_ putStrLn $ terminalMultiNumberedList prompt outputs
      fmap (choices !!) <$> usrChoiceIdxWithCancel maxIdx
    where
         choices = ps
         outputs = let out = (lines . renderStyle (genStyle
                              terminalInnerFramedWidth) . ppr)
                   in fmap (\(ctx, sub) -> out (Bind [ctxKindToChar k] ctx 0)
                                        ++ out (Bind "M" sub 0)) choices
         maxIdx  = length choices - 1


ctxSubChoices' :: [(Ctx, [Term])]
                 -> CtxKind
                 -> String
                 -> InterM InterEnv (Maybe (Ctx, [Term]))
ctxSubChoices' [] _ _ = return Nothing
ctxSubChoices' ps k prompt
 =  do
      liftIO $ mapM_ putStrLn $ terminalMultiNumberedList prompt outputs
      fmap (choices !!) <$> usrChoiceIdxWithCancel maxIdx
    where
         choices = ps
         outputs = fmap (\(ctx, subs) ->
                    let maxIdx   = length subs
                        maxWidth = length $ show maxIdx
                        m i      = let si = show (i :: Int)
                                   in "M" ++ si ++ replicate (maxWidth - length si) ' '
                    in out (Bind (ctxKindToChar k : replicate maxWidth ' ') ctx 0)
                    ++ concatMap (\(i, sub) -> out (Bind (m i) sub 0)) (zip [0..] subs)) choices
         maxIdx  = length choices - 1
         out     = lines . renderStyle (genStyle terminalInnerFramedWidth) . ppr
