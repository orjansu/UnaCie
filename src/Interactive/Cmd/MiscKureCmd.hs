
module MiscKureCmd
  ( interp_rotateBindingsC    -- Rotate let bindings clockwise.
  , interp_rotateBindingsCC   -- Rotate let bindings counters clockwise.
  
  -- Matchers/refiners for above commands: --

  , matcher_rotateBindingsC
  , matcher_rotateBindingsCC
  , refiner_rotateBindingsC
  , refiner_rotateBindingsCC

  ) where

import CmdAST       (Cmd(..), RawCmd(..))
import CmdError     (CmdError(..), InternalError(..))
import InterUtils   (applyRTermCurrPathLog, outputCmdError)
import InterState   (isTransState)
import CmdParser    (cmdMatcher, cmdMatcherNoParams)
import ParamRefine  (paramsRefine)
import Types        (Interp, Matcher, Refiner)
import MiscRewrites (rotateBindingsCClockwiseR, rotateBindingsClockwiseR)

import Data.Bifunctor (bimap)

{-
  <TO-DO>: N/A

  Information:
  -----------------------------------------------------------------------------
  - Miscellaneous KURE commands.
-}


-- Misc. for let bindings: -----------------------------------------------------

-------------------------------------------------------------------------------
-- rotate-bindings-c: --
-------------------------------------------------------------------------------
-- Rotate let bindings clockwise.

matcher_rotateBindingsC :: Matcher
matcher_rotateBindingsC  = cmdMatcherNoParams "rotate-bindings-c" RawKureCmd

refiner_rotateBindingsC :: Refiner
refiner_rotateBindingsC (RawKureCmd "rotate-bindings-c" ps) =  
  bimap ParamErr (KureCmd "rotate-bindings-c") $ paramsRefine ps [[]]
refiner_rotateBindingsC _ = Left $ InternalErr $ WrongRefine "refiner_rotateBindingsC"

interp_rotateBindingsC :: Interp
interp_rotateBindingsC cmd@(KureCmd "rotate-bindings-c" ps) mrel st
  | isTransState st = case ps of
      [] ->  applyRTermCurrPathLog (cmd, mrel) rotateBindingsClockwiseR
      _  -> outputCmdError $ InternalErr $ UnexpectedParams
              "interp_rotateBindingsC" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
interp_rotateBindingsC _ _ _ = 
  outputCmdError $ InternalErr $ WrongInter "interp_rotateBindingsC"

-------------------------------------------------------------------------------
-- rotate-bindings-cc: --
-------------------------------------------------------------------------------
-- Rotate let bindings counter clockwise.

matcher_rotateBindingsCC :: Matcher
matcher_rotateBindingsCC  = cmdMatcher "rotate-bindings-cc" RawKureCmd [[]]

refiner_rotateBindingsCC :: Refiner
refiner_rotateBindingsCC (RawKureCmd "rotate-bindings-cc" ps) =
  bimap ParamErr (KureCmd "rotate-bindings-cc") $ paramsRefine ps [[]]
refiner_rotateBindingsCC _ = 
  Left $ InternalErr $ WrongRefine  "refiner_rotateBindingsCC"

interp_rotateBindingsCC :: Interp
interp_rotateBindingsCC cmd@(KureCmd "rotate-bindings-cc" ps) mrel st
  | isTransState st  = case ps of
      [] -> applyRTermCurrPathLog (cmd, mrel) rotateBindingsCClockwiseR
      _  -> outputCmdError $ InternalErr $ UnexpectedParams
              "interp_rotateBindingsCC" $ fmap show ps
  | otherwise = outputCmdError (StateErr st)
interp_rotateBindingsCC _ _ _ = 
  outputCmdError $ InternalErr $ WrongInter "interp_rotateBindingsCC"