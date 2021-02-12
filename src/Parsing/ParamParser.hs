
module ParamParser
  ( cmdNameMatcher   -- Match a command name parameter.
  , fileMatcher      -- Match a file parameter.
  , numberMatcher    -- Match a number parameter.
  , paramMatcher     -- Generarte a parameter matcher.
  , propMatcher      -- Match a proposition parameter.
  , srcCodeMatcher   -- Match a source code parameter.
  , srcNameMatcher   -- Match a source name parameter.
  ) where

import CmdAST       (LocatedRawParam(..), RawParam(..))
import CmdError     (ParamError(..))
import ParsingUtils (expectedError)
import CmdLexUtils  (LocatedToken(..))
import qualified CmdLexUtils as L

import Data.Either   (isRight, lefts, rights)
import Data.List     (null, partition)
import Control.Monad (msum)

{-
  Information:
  -----------------------------------------------------------------------------
  - We parse a command's list of parameters with a list of matchers which is
    zipped with it. Each matcher is responsible for precisely one param.
    and either parses the param. successfully, converting it from a
    LocatedToken to a LocatedRawParam, or returns a parameter error typically
    specifying the type of its /expected/ parameter;
  - We also take into consideration additional and missing parameters and
    handle them uniformly
  -- Additional parameters are located in the source and the types of missing
     parameters are reported.
-}

-- Top-level function: --------------------------------------------------------

paramMatcher :: [Maybe LocatedToken -> Either ParamError LocatedRawParam]
                -> [LocatedToken]
                -> Either [ParamError] [LocatedRawParam]
paramMatcher fs lts | null fail = Right (rights succ)
                    | otherwise = Left  (lefts fail)
  where
    (succ, fail) = partition isRight (zipWith ($) fs' lts')
    (fs', lts')  = ( take n $ fs ++ repeat extraParamMatcher
                   , take n $ fmap Just lts ++ repeat Nothing )
    n = max (length fs) (length lts)

-- Parameter matchers: --------------------------------------------------------
-- We have one for each token, except CmdSep: --

-- Reports extra parameters.
extraParamMatcher :: Maybe LocatedToken -> Either ParamError LocatedRawParam
extraParamMatcher (Just (LocatedToken t pos)) = Left $ ExtraParam (show t) pos
-- In practice this should /never/ happen.
extraParamMatcher Nothing = Left $ MissingParam "?"

-- RawCmdName.
cmdNameMatcher :: Maybe LocatedToken -> Either ParamError LocatedRawParam
cmdNameMatcher (Just (LocatedToken t pos)) =
  maybe (Left $ InvalidParam (expectedError (show t) "CmdName") pos)
   Right (tokToRawCmdName t pos)
cmdNameMatcher Nothing = Left $ MissingParam "CmdName"

-- RawSrcCode.
srcCodeMatcher :: Maybe LocatedToken -> Either ParamError LocatedRawParam
srcCodeMatcher (Just (LocatedToken t pos)) =
  maybe (Left $ InvalidParam (expectedError (show t) "SrcCode") pos)
   Right (tokToRawSrcCode t pos)
srcCodeMatcher Nothing = Left $ MissingParam "SrcCode"

-- RawSrcName.
srcNameMatcher :: Maybe LocatedToken -> Either ParamError LocatedRawParam
srcNameMatcher (Just (LocatedToken t pos)) =
  maybe (Left $ InvalidParam (expectedError (show t) "SrcName") pos)
   Right (tokToRawSrcName t pos)
srcNameMatcher Nothing = Left $ MissingParam "SrcName"

-- RawNumber.
numberMatcher :: Maybe LocatedToken -> Either ParamError LocatedRawParam
numberMatcher (Just (LocatedToken t pos)) =
  maybe (Left $ InvalidParam (expectedError (show t) "Number") pos)
   Right (tokToRawNumber t pos)
numberMatcher Nothing = Left $ MissingParam "Number"

-- RawFile.
fileMatcher :: Maybe LocatedToken -> Either ParamError LocatedRawParam
fileMatcher (Just (LocatedToken t pos)) =
  maybe (Left $ InvalidParam (expectedError (show t) "File") pos)
   Right (tokToRawFile t pos)
fileMatcher Nothing = Left $ MissingParam "File"

-- RawProp.
propMatcher :: Maybe LocatedToken -> Either ParamError LocatedRawParam
propMatcher (Just (LocatedToken t@(L.Prop t1 t2 t3) pos)) =
  maybe (Left $ InvalidParam (expectedError (show t) "Prop") pos)
   Right $ do
     lp1 <- tokToRawSrc     t1 pos
     lp2 <- tokToRawCmdName t2 pos
     lp3 <- tokToRawSrc     t3 pos
     return $ LocatedRawParam (RawProp (par lp1) (par lp2) (par lp3)) pos
propMatcher (Just (LocatedToken t pos)) =
  Left $ InvalidParam (expectedError (show t) "Prop") pos
propMatcher Nothing = Left $ MissingParam "Prop"

-- Helpers: -------------------------------------------------------------------

-- RawCmdName.
tokToRawCmdName :: L.Token -> L.Pos -> Maybe LocatedRawParam
tokToRawCmdName (L.CmdName s) pos = Just $ LocatedRawParam (RawCmdName s) pos
tokToRawCmdName _ _ = Nothing

-- RawSrcCode.
tokToRawSrcCode ::  L.Token -> L.Pos -> Maybe LocatedRawParam
tokToRawSrcCode (L.SrcCode s) pos = Just $ LocatedRawParam (RawSrcCode s) pos
tokToRawSrcCode _ _ = Nothing

-- RawSrcName.
tokToRawSrcName ::  L.Token -> L.Pos -> Maybe LocatedRawParam
tokToRawSrcName (L.SrcName s) pos = Just $ LocatedRawParam (RawSrcName s) pos
tokToRawSrcName _ _ = Nothing

-- RawSrc = RawSrcName + RawSrcCode.
tokToRawSrc  :: L.Token -> L.Pos -> Maybe LocatedRawParam
tokToRawSrc t p = msum [tokToRawSrcName t p, tokToRawSrcCode t p]

-- RawNumber.
tokToRawNumber :: L.Token -> L.Pos -> Maybe LocatedRawParam
tokToRawNumber (L.Number i) pos = Just $ LocatedRawParam (RawNumber i) pos
tokToRawNumber _ _ = Nothing

-- RawFile.
tokToRawFile :: L.Token -> L.Pos -> Maybe LocatedRawParam
tokToRawFile (L.File s) pos = Just $ LocatedRawParam (RawFile s) pos
tokToRawFile _ _ = Nothing
