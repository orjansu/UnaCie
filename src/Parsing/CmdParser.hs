
module CmdParser
  ( parse               -- Parse a single command given.
  , parse'              -- Parse a sequence of command.
  , parseScript         -- Parse a command script.
  , cmdMatcher          -- Build command parsers 'componentwise'.
  , cmdMatcherNoParams  -- As above but for commands without params.
  ) where 

import CmdAST        (CmdName, LocatedRawParam(..), RawCmd(..))
import CmdError      ( CmdError(..), InternalError(..)
                     , ParamError(..), sortParamErrors )
import CmdLexUtils   ( LexError(..), LocatedToken(..)
                     , Pos(..), Token(..) )
import CmdLexer      ( AlexPosn(..), AlexReturn(..) 
                     , alexScan, alexStartPos )
import PPLib         (ppr)
import ParamParser   (paramMatcher)
import PrintSettings (terminalLineStyle)
import Utils         (notNull, prepStr)

import Prelude hiding (lex)
import Control.Monad  ((>=>))
import Data.Bifunctor (bimap)
import Data.Either    (isRight, lefts, rights)
import Data.List      (partition, permutations)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), empty, renderStyle)

{-
  Information:
  -----------------------------------------------------------------------------
  - Module to handle parsing /raw/ commands from input on the command line/
    scripts;
  - A raw command is parsed from a string as follows:
    (1) String       -> LocatedToken: a string is lexed into a list of tokens.
    (2) LocatedToken -> LocatedToken: sequences of tokens corresponding to
                        individual commands are separated by splitting at 
                        CmdSep tokens.
    (3) LocatedToken -> RawCmd: tokens are parsed by cmd./param. matchers.
-}

-- Top-level functions: -------------------------------------------------------

-- Parse a string input on the command line as follows:
-- (1) Lex;
-- (2) Check sequence of tokens starts with a CmdName;
-- (3) Parse tokens to RawCmd using matchers.
-- Note we have to ensure that /only one/ matcher successfully parsers
-- the command. If multiple matchers succeed, we fail.
parse :: [[LocatedToken] -> [(Either CmdError RawCmd)]] 
         -> String 
         -> Either CmdError RawCmd
parse matchers str = 
  (parseFailCheck 
   . (matchers >>=) 
   . flip ($)) =<< (lex >=> checkCmdLToks) str     
  where 
    parseFailCheck []      = Left $ InvalidCmd str (Pos 0 0)
    parseFailCheck [x]     = x 
    parseFailCheck (_ : _) = Left (InternalErr MultiParseSuccess)


-- As above, but for a sequence of (comma/newline separated) commands.
parse' :: [[LocatedToken] -> [(Either CmdError RawCmd)]] 
          -> String 
          -> Either [CmdError] [RawCmd]
parse' matchers str = 
  parseToks . fmap checkCmdLToks . sepCmdLToks =<< bimap return id (lex str)
  where
    parseToks []     = Left [InvalidCmd "no command(s) to parse" (Pos 0 0)]
    parseToks lTokss | all isRight lTokss = 
      let parseOut = [ parseFailCheck (concatMap ($ lToks) matchers, lToks)
                     | lToks <- rights lTokss ]
      in if all isRight parseOut
            then Right (rights parseOut)
            else Left  (lefts parseOut)
    parseToks lTokss = Left (lefts lTokss)

-- As parse' but with script errors
parseScript :: [[LocatedToken] -> [(Either CmdError RawCmd)]] 
               -> String 
               -> Either CmdError [RawCmd]
parseScript matchers str = 
  parseToks 
   . fmap checkCmdLToks 
   . sepCmdLToks =<< bimap lexToScriptError id (lex str)
  where 
   parseToks []     = Left (ScriptErr "empty script." [])
   parseToks lTokss | all isRight lTokss
    = let parseOut = [ parseFailCheck (concatMap ($ lToks) matchers, lToks)
                     | lToks <- rights lTokss ]
      in if all isRight parseOut
            then Right (rights parseOut)
            else Left $ ScriptErr "one or more \
                  \command errors" (lefts parseOut)
   parseToks lTokss = Left $ ScriptErr "one or more \
                  \command errors" (lefts lTokss)
   
   -- Lexical error -> script error
   lexToScriptError err = ScriptErr "lexical error" [err]

-- Helpers: -------------------------------------------------------------------

-- Check if none/one/multiple matchers have successfully parsed 
-- a list of tokens and report accordingly
parseFailCheck :: ([Either CmdError b], [LocatedToken]) -> Either CmdError b
parseFailCheck ([], lToks) = case lToks of 
  []       -> Left $ InvalidCmd (renderStyle terminalLineStyle 
              $ printLToks lToks) (Pos 0 0)
  (lt : _) -> Left $ InvalidCmd (renderStyle terminalLineStyle
              $ printLToks lToks) (CmdLexUtils.pos lt)
parseFailCheck ([x], _)     = x 
parseFailCheck ((_ : _), _) = Left (InternalErr MultiParseSuccess)

-- Print location info. from first ltok only.
printLToks :: [LocatedToken] -> Doc 
printLToks []         = empty
printLToks (lt : lts) = ppr lt <+> ppr (fmap tok lts)

-- Command matcher: -----------------------------------------------------------
{- 
   - Build a command matcher using a command name, RawCmd function g
     and param. matchers fss;
   - Permute the parameters to allow them to be input in any order
   -- If this causes a command to be parsed successfully in multiple ways, 
      we take the first one: this corresponds to the order of the arguments
      /as they appear on the command line/.
   - If we return a command error, we attempt to give the most appropriate
     one by 'sorting' them, see CmdError for details.
-}
cmdMatcher :: CmdName 
              -> (CmdName -> [LocatedRawParam] -> RawCmd)
              -> [[Maybe LocatedToken -> Either ParamError LocatedRawParam]]
              -> [LocatedToken] 
              -> [(Either CmdError RawCmd)] 
cmdMatcher _ _ _ [] = []                                                        
cmdMatcher name g fss (LocatedToken t _ : params) = 
  if cmdNameMatch t 
     then case succ of 
       Right ps : _ -> return . Right $ g name ps
       _            -> return . Left . ParamErr . sortParamErrors . lefts $ fail 
     else []
  where 
    -- prepStr to lower and remove any surrounding whitespace.
    cmdNameMatch (CmdName name') = name == prepStr name' 
    cmdNameMatch _ = False

    -- Parse the command's parameters, see ParamParser for details.
    -- Note that we permute the parameters try and allow any order to 
    -- be parsed successfully.
    (succ, fail) = partition isRight [ paramMatcher fs ps 
                                     | fs <- fss
                                     , ps <- permutations params ]

-- As above but when for commands that require no parameters.
cmdMatcherNoParams :: CmdName 
                      -> (CmdName -> [LocatedRawParam] -> RawCmd)
                      -> [LocatedToken] 
                      -> [(Either CmdError RawCmd)]  
cmdMatcherNoParams name g = cmdMatcher name g [[]]

-- Command Lexing: ------------------------------------------------------------

-- See CmdLexer.x for lexer specification.
-- Also see Alex lexer documentation for details on implementations.
lex :: String -> Either CmdError [LocatedToken]
lex s = go (alexStartPos, '\n', [], s)
  where go inp@(pos, _, _, str) = case alexScan inp 0 of
          AlexEOF -> Right []
          AlexError ((AlexPn _ l c), pc, _, _) ->
            Left $ LexErr $ LexError { ePos = Pos { lineNo = l
                                                  , colNo = c } 
                                     , curStr   = str
                                     , prevChar = pc }
          AlexSkip  inp _  -> go inp
          AlexToken inp len act -> (act pos (take len str) :) <$> go inp

-- Helpers: -------------------------------------------------------------------

-- Separate ltoks relating to individual commands by breaking at CmdSeps.
sepCmdLToks :: [LocatedToken] -> [[LocatedToken]]
sepCmdLToks  = filter notNull . go 
               where 
                 go [] = [] 
                 go xs = takeWhile notSep xs 
                         : go (drop 1 $ dropWhile notSep xs)
                 notSep (LocatedToken t _) = t /= CmdSep

-- Make sure a sequence of tokens are of the form <cmd-name> <cmd-param>*.
-- I.e., a valid command sequence.
checkCmdLToks :: [LocatedToken] -> Either CmdError [LocatedToken]
checkCmdLToks [] = Right []
checkCmdLToks lToks@(lt : _) | isName (tok lt) = Right lToks
  where 
    isName CmdName{} = True 
    isName _         = False
checkCmdLToks (lt : _) = Left (InvalidCmd "commands s/be: \
  \<name> <param>*." (CmdLexUtils.pos lt))