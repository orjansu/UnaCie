{-# LANGUAGE LambdaCase #-}

module TabulateKureCmd where

import CmdAST
import CmdError
import InterUtils
import InterState (isTransState)
import CmdParser
import ParamRefine
import Types

-- Rewrites
import TabulateRewrites

import Data.Bifunctor (bimap)


-------------------------------------------------------------------------------
-- plus-right-ident: --
-------------------------------------------------------------------------------

matcher_plusRightIdent :: Matcher
matcher_plusRightIdent  = cmdMatcherNoParams "plus-right-ident" RawKureCmd

refiner_plusRightIdent :: Refiner
refiner_plusRightIdent (RawKureCmd "plus-right-ident" ps) =
  bimap ParamErr (KureCmd "plus-right-ident") $ paramsRefine ps [[]]
refiner_plusRightIdent _ = Left $ InternalErr $ WrongRefine "plus-right-ident"

interp_plusRightIdent :: Interp
interp_plusRightIdent cmd@(KureCmd "plus-right-ident" ps) mrel st
  | isTransState st = case ps of

     [] -> applyRTermCurrPathLog (cmd, mrel) plusRightIdentR

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_plusRightIdent" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

-- Error case.
interp_plusRightIdent _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_plusRightIdent"


-------------------------------------------------------------------------------
-- plus-left-ident: --
-------------------------------------------------------------------------------

matcher_plusLeftIdent :: Matcher
matcher_plusLeftIdent  = cmdMatcherNoParams "plus-left-ident" RawKureCmd

refiner_plusLeftIdent :: Refiner
refiner_plusLeftIdent (RawKureCmd "plus-left-ident" ps) =
  bimap ParamErr (KureCmd "plus-left-ident") $ paramsRefine ps [[]]
refiner_plusLeftIdent _ = Left $ InternalErr $ WrongRefine "plus-left-ident"

interp_plusLeftIdent :: Interp
interp_plusLeftIdent cmd@(KureCmd "plus-left-ident" ps) mrel st
  | isTransState st = case ps of

     [] -> applyRTermCurrPathLog (cmd, mrel) plusLeftIdentR

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_plusLeftIdent" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

-- Error case.
interp_plusLeftIdent _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_plusLeftIdent"


-------------------------------------------------------------------------------
-- plus-assoc: --
-------------------------------------------------------------------------------

matcher_plusAssoc :: Matcher
matcher_plusAssoc  = cmdMatcherNoParams "plus-assoc" RawKureCmd

refiner_plusAssoc :: Refiner
refiner_plusAssoc (RawKureCmd "plus-assoc" ps) =
  bimap ParamErr (KureCmd "plus-assoc") $ paramsRefine ps [[]]
refiner_plusAssoc _ = Left $ InternalErr $ WrongRefine "plus-assoc"

interp_plusAssoc :: Interp
interp_plusAssoc cmd@(KureCmd "plus-assoc" ps) mrel st
  | isTransState st = case ps of

     [] -> do
      lib <- getInterEnv getCtxEqs
      applyRTermCurrPathLog (cmd, mrel) (plusAssocGenR lib)

     -- Any other params. are invalid: ----------------------------------------
     _  -> outputCmdError $ InternalErr $ UnexpectedParams "interp_plusAssoc" $ fmap show ps

  -- Invalid state as not transformation.
  | otherwise = outputCmdError (StateErr st)

-- Error case.
interp_plusAssoc _ _ _ = outputCmdError $ InternalErr $ WrongInter "interp_plusAssoc"
