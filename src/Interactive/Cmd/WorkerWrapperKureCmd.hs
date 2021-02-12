{-# LANGUAGE LambdaCase #-}

module WorkerWrapperKureCmd where

import CtxAST
import CtxKind
import CtxPatAST
import PPLib
import InterPrintUtils
import CmdAST
import CmdError
import InterEnv
import InterUtils
import InterState (isTransState)
import CmdParser
import ParamParser
import ParamRefine
import Types
import Universes

-- Rewrites
import WorkerWrapperRewrites

import Data.Bifunctor (bimap)
import Control.Monad.IO.Class
import Text.PrettyPrint.HughesPJ




-------------------------------------------------------------------------------
-- ww-ass-a: --
-------------------------------------------------------------------------------

matcher_wwAssA :: Matcher
matcher_wwAssA  = cmdMatcher "ww-ass-a" RawKureCmd
  [[], [srcCodeMatcher], [srcCodeMatcher, srcCodeMatcher]]

refiner_wwAssA :: Refiner
refiner_wwAssA (RawKureCmd "ww-ass-a" ps) =
  bimap ParamErr (KureCmd "ww-ass-a") $ paramsRefine ps
   [[], [ctxPatSrcCodeRefine], [ctxSrcCodeRefine, ctxSrcCodeRefine]]
refiner_wwAssA _ = Left $ InternalErr $ WrongRefine "refiner_wwAssA"

interp_wwAssA :: Interp
interp_wwAssA cmd@(KureCmd "ww-ass-a" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (wwAssAGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid value context nestings.")
         [(c1, c2, _)] -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (wwAssAR lib c1 c2)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
      lib <- getInterEnv getCtxEqs
      applyTTermCurrPathUpdate (wwAssAMatchT lib pctx) $ \case
       [] -> outputCmdError (KureErr "invalid context pattern.")
       [(c1, c2, _)] -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (wwAssAR lib c1 c2 )
       ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (wwAssAR lib c1 c2)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_wwAssA" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

  where
    -- Generate the full command to be logged in UNIE's history.
   genCmd c1 c2 = KureCmd "ww-ass-a" [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices3 ps VAL VAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (c1, c2, _) -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (wwAssAR lib c1 c2)

-- Error case.
interp_wwAssA _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_wwAssA"


-------------------------------------------------------------------------------
-- unww-ass-a: --
-------------------------------------------------------------------------------

matcher_unwwAssA :: Matcher
matcher_unwwAssA  = cmdMatcher "unww-ass-a" RawKureCmd  [[srcCodeMatcher, srcCodeMatcher]]

refiner_unwwAssA :: Refiner
refiner_unwwAssA (RawKureCmd "unww-ass-a" ps) =
  bimap ParamErr (KureCmd "unww-ass-a") $ paramsRefine ps [[ctxSrcCodeRefine, ctxSrcCodeRefine]]
refiner_unwwAssA _ = Left $ InternalErr $ WrongRefine "refiner_unwwAssA"

interp_unwwAssA :: Interp
interp_unwwAssA cmd@(KureCmd "unww-ass-a" ps) mrel st
  | isTransState st = case ps of

     [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (unwwAssAR lib c1 c2)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_unwwAssA" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

-- Error case.
interp_unwwAssA _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unwwAssA"



-------------------------------------------------------------------------------
-- ww-ass-c: --
-------------------------------------------------------------------------------

matcher_wwAssC :: Matcher
matcher_wwAssC  = cmdMatcher "ww-ass-c" RawKureCmd
  [[], [srcCodeMatcher], [srcCodeMatcher, srcCodeMatcher]]

refiner_wwAssC :: Refiner
refiner_wwAssC (RawKureCmd "ww-ass-c" ps) =
  bimap ParamErr (KureCmd "ww-ass-c") $ paramsRefine ps
   [[], [ctxPatSrcCodeRefine], [ctxSrcCodeRefine, ctxSrcCodeRefine, ctxSrcCodeRefine]]
refiner_wwAssC _ = Left $ InternalErr $ WrongRefine "refiner_wwAssC"

interp_wwAssC :: Interp
interp_wwAssC cmd@(KureCmd "ww-ass-c" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (wwAssCGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid value context nestings.")
         [(c1, c2, c3, _)] -> applyRTermCurrPathLog (genCmd c1 c2 c3, mrel) (wwAssCR lib c1 c2 c3)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
      lib <- getInterEnv getCtxEqs
      applyTTermCurrPathUpdate (wwAssCMatchT lib pctx) $ \case
       [] -> outputCmdError (KureErr "invalid context pattern.")
       [(c1, c2, c3, _)] -> applyRTermCurrPathLog (genCmd c1 c2 c3, mrel) (wwAssCR lib c1 c2 c3)
       ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2), CtxSrcCode (UCtx c3)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (wwAssCR lib c1 c2 c3)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_wwAssC" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

  where
    -- Generate the full command to be logged in UNIE's history.
   genCmd c1 c2 c3 = KureCmd "ww-ass-c" [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2), CtxSrcCode (UCtx c3)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices4 ps VAL VAL VAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (c1, c2, c3, _) -> applyRTermCurrPathLog (genCmd c1 c2 c3, mrel) (wwAssCR lib c1 c2 c3)

-- Error case.
interp_wwAssC _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_wwAssC"



-------------------------------------------------------------------------------
-- unww-ass-c: --
-------------------------------------------------------------------------------

matcher_unwwAssC :: Matcher
matcher_unwwAssC  = cmdMatcher "unww-ass-c" RawKureCmd
  [[srcCodeMatcher, srcCodeMatcher], [srcCodeMatcher, srcCodeMatcher, srcCodeMatcher]]

refiner_unwwAssC :: Refiner
refiner_unwwAssC (RawKureCmd "unww-ass-c" ps) =
  bimap ParamErr (KureCmd "unww-ass-c") $ paramsRefine ps
   [[ctxPatSrcCodeRefine, ctxSrcCodeRefine, ctxSrcCodeRefine],
   [ctxSrcCodeRefine, ctxSrcCodeRefine], [ctxSrcCodeRefine, ctxSrcCodeRefine, ctxSrcCodeRefine]]
refiner_unwwAssC _ = Left $ InternalErr $ WrongRefine "refiner_unwwAssC"

interp_unwwAssC :: Interp
interp_unwwAssC cmd@(KureCmd "unww-ass-c" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (unwwAssCGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid value context nestings.")
         [(c3, _)] -> applyRTermCurrPathLog (genCmd c1 c2 c3, mrel) (unwwAssCR lib c1 c2 c3)
         ps -> userChoice ps c1 c2 lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx), CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)] -> do
      lib <- getInterEnv getCtxEqs
      applyTTermCurrPathUpdate (unwwAssCMatchT lib pctx) $ \case
       [] -> outputCmdError (KureErr "invalid context pattern.")
       [(c3, _)] -> applyRTermCurrPathLog (genCmd c1 c2 c3, mrel) (unwwAssCR lib c1 c2 c3)
       ps -> userChoice ps c1 c2 lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2), CtxSrcCode (UCtx c3)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (unwwAssCR lib c1 c2 c3)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_unwwAssC" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

  where
    -- Generate the full command to be logged in UNIE's history.
   genCmd c1 c2 c3 = KureCmd "unww-ass-c" [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2), CtxSrcCode (UCtx c3)]

   -- Ask user to select a context option, or cancel.
   userChoice ps c1 c2 lib = ctxSubChoices ps VAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (c3, _) -> applyRTermCurrPathLog (genCmd c1 c2 c3, mrel) (unwwAssCR lib c1 c2 c3)

-- Error case.
interp_unwwAssC _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unwwAssC"





-------------------------------------------------------------------------------
-- rolling-rule: --
-------------------------------------------------------------------------------

matcher_rollingRule :: Matcher
matcher_rollingRule  = cmdMatcher "rolling-rule" RawKureCmd
  [[], [srcCodeMatcher], [srcCodeMatcher, srcCodeMatcher]]

refiner_rollingRule :: Refiner
refiner_rollingRule (RawKureCmd "rolling-rule" ps) =
  bimap ParamErr (KureCmd "rolling-rule") $ paramsRefine ps
   [[], [ctxPatSrcCodeRefine], [ctxSrcCodeRefine, ctxSrcCodeRefine]]
refiner_rollingRule _ = Left $ InternalErr $ WrongRefine "refiner_rollingRule"

interp_rollingRule:: Interp
interp_rollingRule cmd@(KureCmd "rolling-rule" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (rollingRuleGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid value context nestings.")
         [(c1, c2, _)] -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (rollingRuleR lib c1 c2)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
      lib <- getInterEnv getCtxEqs
      applyTTermCurrPathUpdate (rollingRuleMatchT lib pctx) $ \case
       [] -> outputCmdError (KureErr "invalid context pattern.")
       [(c1, c2, _)] -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (rollingRuleR lib c1 c2 )
       ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (rollingRuleR lib c1 c2)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_rollingRule" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

  where
    -- Generate the full command to be logged in UNIE's history.
   genCmd c1 c2 = KureCmd "rolling-rule" [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices3 ps VAL VAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (c1, c2, _) -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (rollingRuleR lib c1 c2)

-- Error case.
interp_rollingRule _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_rollingRule"


-------------------------------------------------------------------------------
-- unrolling-rule: --
-------------------------------------------------------------------------------

matcher_unrollingRule :: Matcher
matcher_unrollingRule  = cmdMatcher "unrolling-rule" RawKureCmd
  [[], [srcCodeMatcher], [srcCodeMatcher, srcCodeMatcher]]

refiner_unrollingRule :: Refiner
refiner_unrollingRule (RawKureCmd "unrolling-rule" ps) =
  bimap ParamErr (KureCmd "unrolling-rule") $ paramsRefine ps
   [[], [ctxPatSrcCodeRefine], [ctxSrcCodeRefine, ctxSrcCodeRefine]]
refiner_unrollingRule _ = Left $ InternalErr $ WrongRefine "refiner_unrollingRule"

interp_unrollingRule:: Interp
interp_unrollingRule cmd@(KureCmd "unrolling-rule" ps) mrel st
  | isTransState st = case ps of

     -- Generate context options on behalf of the user: -----------------------

     [] -> do
       lib <- getInterEnv getCtxEqs
       applyTTermCurrPathUpdate (unrollingRuleGenT lib) $ \case
         [] -> outputCmdError (KureErr "no valid value context nestings.")
         [(c1, c2, _)] -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (unrollingRuleR lib c1 c2)
         ps -> userChoice ps lib

     -- Match the context option from a specific context pattern: -------------

     [CtxPatSrcCode (UCtxPat pctx)] -> do
      lib <- getInterEnv getCtxEqs
      applyTTermCurrPathUpdate (unrollingRuleMatchT lib pctx) $ \case
       [] -> outputCmdError (KureErr "invalid context pattern.")
       [(c1, c2, _)] -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (unrollingRuleR lib c1 c2 )
       ps -> userChoice ps lib

     -- Use the given context: ------------------------------------------------

     [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)] -> do
       lib <- getInterEnv getCtxEqs
       applyRTermCurrPathLog (cmd, mrel) (unrollingRuleR lib c1 c2)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_unrollingRule" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

  where
    -- Generate the full command to be logged in UNIE's history.
   genCmd c1 c2 = KureCmd "unrolling-rule" [CtxSrcCode (UCtx c1), CtxSrcCode (UCtx c2)]

   -- Ask user to select a context option, or cancel.
   userChoice ps lib = ctxSubChoices3 ps VAL VAL "Select a context/substitution option:" >>= \case
     Nothing -> interPutInfo "cancelled."
     Just (c1, c2, _) -> applyRTermCurrPathLog (genCmd c1 c2, mrel) (unrollingRuleR lib c1 c2)

-- Error case.
interp_unrollingRule _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_unrollingRule"







-- Helpers: -------------------------------------------------------------------

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

ctxSubChoices3 :: [(Ctx, Ctx, Term)]
                  -> CtxKind
                  -> CtxKind
                  -> String
                  -> InterM InterEnv (Maybe (Ctx, Ctx, Term))
ctxSubChoices3 [] _ _ _ = return Nothing
ctxSubChoices3 ps k1 k2 prompt
 =  do
      liftIO $ mapM_ putStrLn $ terminalMultiNumberedList prompt outputs
      fmap (choices !!) <$> usrChoiceIdxWithCancel maxIdx
    where
         choices = ps
         outputs = fmap (\(c1, c2, sub) ->

                    out (Bind [ctxKindToChar k1] c1 0)
                    ++ out (Bind [ctxKindToChar k2] c2 0)
                    ++ out (Bind "M" sub 0)) choices

         maxIdx  = length choices - 1
         out     = lines . renderStyle (genStyle terminalInnerFramedWidth) . ppr


ctxSubChoices4 :: [(Ctx, Ctx, Ctx, Term)]
                  -> CtxKind
                  -> CtxKind
                  -> CtxKind
                  -> String
                  -> InterM InterEnv (Maybe (Ctx, Ctx, Ctx, Term))
ctxSubChoices4 [] _ _ _ _ = return Nothing
ctxSubChoices4 ps k1 k2 k3 prompt
 =  do
      liftIO $ mapM_ putStrLn $ terminalMultiNumberedList prompt outputs
      fmap (choices !!) <$> usrChoiceIdxWithCancel maxIdx
    where
         choices = ps
         outputs = fmap (\(c1, c2, c3, sub) ->

                    out (Bind [ctxKindToChar k1] c1 0)
                    ++ out (Bind [ctxKindToChar k2] c2 0)
                    ++ out (Bind [ctxKindToChar k3] c3 0)
                    ++ out (Bind "M" sub 0)) choices

         maxIdx  = length choices - 1
         out     = lines . renderStyle (genStyle terminalInnerFramedWidth) . ppr
