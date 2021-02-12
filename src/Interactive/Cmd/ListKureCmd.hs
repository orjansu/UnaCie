{-# LANGUAGE LambdaCase #-}

module ListKureCmd
  ( interp_appAssoc_LR  -- Reassociate append from left to right.
  , interp_appAssoc_RL  -- Reassociate append from right to left.
  , interp_appIdent     -- Append's left identity.

  -- Matchers/refiners for above commands: --

  , matcher_appAssoc_LR_I
  , matcher_appAssoc_LR_WCE
  , matcher_appAssoc_RL_WCE
  , matcher_appAssoc_RL_WI
  , matcher_appIdent
  , refiner_appAssoc_LR
  , refiner_appAssoc_RL
  , refiner_appIdent

  ) where

import CmdAST       (Cmd(..), RawCmd(..))
import CmdError     (CmdError(..), InternalError(..))
import CmdParser    (cmdMatcherNoParams)
import InterState   (isTransState)
import InterUtils   (applyRTermCurrPathLog, outputCmdError)
import ListRewrites (appendIdentR, appendAssoc_LR_R, appendAssoc_RL_R)
import ParamRefine  (paramsRefine)
import Types        (Interp, Matcher, Refiner)

import Data.Bifunctor (bimap)

{-
  Information:
  -----------------------------------------------------------------------------
  - KURE commands that transform lists.
-}

-------------------------------------------------------------------------------
-- append-assoc-lr-i/append-assoc-lr-wce: --
-------------------------------------------------------------------------------
-- Reassociating append from left to right.

appAssocLRCmds :: [String]
appAssocLRCmds  = ["append-assoc-lr-i", "append-assoc-lr-wce"]

matcher_appAssoc_LR_I :: Matcher
matcher_appAssoc_LR_I  = cmdMatcherNoParams "append-assoc-lr-i" RawKureCmd

matcher_appAssoc_LR_WCE :: Matcher
matcher_appAssoc_LR_WCE  = cmdMatcherNoParams "append-assoc-lr-wce" RawKureCmd

refiner_appAssoc_LR :: Refiner
refiner_appAssoc_LR (RawKureCmd "append-assoc-lr-i" ps) =
  bimap ParamErr (KureCmd "append-assoc-lr-i") $ paramsRefine ps [[]]
refiner_appAssoc_LR (RawKureCmd "append-assoc-lr-wce" ps) =
  bimap ParamErr (KureCmd "append-assoc-lr-wce") $ paramsRefine ps [[]]
refiner_appAssoc_LR _ = Left $ InternalErr $ WrongRefine "refiner_appAssoc_LR"

interp_appAssoc_LR :: Interp
interp_appAssoc_LR cmd@(KureCmd s ps) mrel st
  | s `elem` appAssocLRCmds && isTransState st && null ps =
      applyRTermCurrPathLog (cmd, mrel) appendAssoc_LR_R
  | s `elem` appAssocLRCmds  && isTransState st =
      outputCmdError $ InternalErr $ UnexpectedParams
       "interp_appAssoc_LR" $ fmap show ps
  | s `elem` appAssocLRCmds = outputCmdError (StateErr st)
interp_appAssoc_LR _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_appAssoc_LR"


-------------------------------------------------------------------------------
-- append-assoc-rl-wi/append-assoc-rl-wce: --
-------------------------------------------------------------------------------
-- Reassociating append from right to left.

appAssocRLCmds :: [String]
appAssocRLCmds  = ["append-assoc-rl-wi", "append-assoc-rl-wce"]

matcher_appAssoc_RL_WI :: Matcher
matcher_appAssoc_RL_WI  = cmdMatcherNoParams "append-assoc-rl-wi" RawKureCmd

matcher_appAssoc_RL_WCE :: Matcher
matcher_appAssoc_RL_WCE  = cmdMatcherNoParams "append-assoc-rl-wce" RawKureCmd

refiner_appAssoc_RL :: Refiner
refiner_appAssoc_RL (RawKureCmd "append-assoc-rl-wi" ps) =
  bimap ParamErr (KureCmd "append-assoc-rl-wi") $ paramsRefine ps [[]]
refiner_appAssoc_RL (RawKureCmd "append-assoc-rl-wce" ps) =
  bimap ParamErr (KureCmd "append-assoc-rl-wce") $ paramsRefine ps [[]]
refiner_appAssoc_RL _ = Left $ InternalErr $ WrongRefine "refiner_appAssoc_RL"

interp_appAssoc_RL :: Interp
interp_appAssoc_RL cmd@(KureCmd s ps) mrel st
  | s `elem` appAssocRLCmds && isTransState st && null ps =
      applyRTermCurrPathLog (cmd, mrel) appendAssoc_RL_R
  | s `elem` appAssocRLCmds  && isTransState st =
      outputCmdError $ InternalErr $ UnexpectedParams
       "interp_appAssoc_RL" $ fmap show ps
  | s `elem` appAssocRLCmds = outputCmdError (StateErr st)
interp_appAssoc_RL _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_appAssoc_RL"


-------------------------------------------------------------------------------
-- append-ident: --
-------------------------------------------------------------------------------

matcher_appIdent :: Matcher
matcher_appIdent  = cmdMatcherNoParams "append-ident" RawKureCmd

refiner_appIdent :: Refiner
refiner_appIdent (RawKureCmd "append-ident" ps) =
  bimap ParamErr (KureCmd "append-ident") $ paramsRefine ps [[]]
refiner_appIdent _ = Left $ InternalErr $ WrongRefine "refiner_appIdent"

interp_appIdent :: Interp
interp_appIdent cmd@(KureCmd "append-ident" ps) mrel st
  | isTransState st = case ps of
     [] -> applyRTermCurrPathLog (cmd, mrel) appendIdentR
     _  -> outputCmdError $ InternalErr $ UnexpectedParams
            "interp_appIdent" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
interp_appIdent _ _ _ =
  outputCmdError $ InternalErr $ WrongInter "interp_appIdent"
