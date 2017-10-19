
{
    
module CmdLexer where 

import CmdLexUtils (LocatedToken(..), Pos(..), Token(..))

import Data.Char       (digitToInt)
import System.FilePath (FilePath)

{-
   Information:
   ------------
   - Lexer for command input;
   - See Alex documentation for more information on lexing.
-}

}

%wrapper "posn"

$digit           =  0-9
$whiteNoNewline  =  $white # \n
$nonWhite        =  ~$white
$nonWhiteNoAp    =  ~$white # \'
$nonQuote        =  . # \"
$nonSrcEnd       =  [\x00-\x10ffff] # \$
$cmdChar         =  [a-z\A-Z\:\-\_]
$opChar          =  [\!\#\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

@srcName  =  \' $nonWhiteNoAp+
@srcCode  =  "$" $nonSrcEnd+ "$"
@src      =  @srcName | @srcCode
@rel      =  I | WI | CE | WCE
@cmdName  =  $cmdChar+ | "(" $opChar+ ")" | $cmdChar+ $digit+

tokens :- 
 
  $whiteNoNewline+         ;

  -- Comments: ----------------------------------------------------------------

  "--" .* | "--" . * \n    ;
    
  -- Prop: --------------------------------------------------------------------

  @src $whiteNoNewline+ 
       @rel 
       $whiteNoNewline+ 
       @src                { toProp }

  -- Command separator: -------------------------------------------------------

  \n | ";"                 { constTokenize CmdSep }

  -- Source name: -------------------------------------------------------------

 --  -- List of source names
 --  (@srcName $white*)+ 
 --                          { \p s -> error $ show s }

  @srcName                 { \p s -> tokenize SrcName p (toSrcName s) }

 
  -- Source code: -------------------------------------------------------------

  @srcCode                 { \p s -> tokenize SrcCode p (toSrc s) }

  -- Filepath: ----------------------------------------------------------------

  "/"      $nonWhite+      { \p s -> tokenize File p (toFp 0 s)     }
  "./"     $nonWhite+      { \p s -> tokenize File p (toFp 2 s)     }
  "../"    $nonWhite+      { \p s -> tokenize File p (toFp 0 s)     }
  \" "/"   $nonQuote+ \"   { \p s -> tokenize File p (toQuotFp 0 s) }
  \" "./"  $nonQuote+ \"   { \p s -> tokenize File p (toQuotFp 2 s) }
  \" "../" $nonQuote+ \"   { \p s -> tokenize File p (toQuotFp 0 s) }

  -- Number: ------------------------------------------------------------------

  $digit+                  { \p s -> tokenize Number p (toInt s) } 

  -- Command name: ------------------------------------------------------------

  @cmdName                 { tokenize CmdName }

{

-- Helpers: -------------------------------------------------------------------

tokenize :: (a -> Token) -> AlexPosn -> a -> LocatedToken
tokenize f p x = LocatedToken { tok = f x, pos = alexPosnToPos p } 

constTokenize :: Token -> AlexPosn -> String -> LocatedToken
constTokenize t p _ = LocatedToken { tok = t, pos = alexPosnToPos p } 

alexPosnToPos :: AlexPosn -> Pos 
alexPosnToPos (AlexPn _ l c) = Pos { lineNo = l, colNo = c }

toProp :: AlexPosn -> String -> LocatedToken
toProp p s = 
  LocatedToken { tok = Prop (srcTok src1) (CmdName rel) (srcTok src2)
               , pos = alexPosnToPos p }
  where 
    srcTok ('\'' : s) = SrcName s 
    srcTok ('$'  : s) = SrcCode $ takeWhile (/= '$') s
    srcTok _          = error "shouldn't happen: toProp"
    [src1, rel, src2] = splitString s

    -- ** This only works for this particular use case. **
    splitString :: String -> [String]
    splitString "" = []
    splitString (' ' : s) = splitString s
    splitString s@('$' : rest) = split : splitString (drop (length split) s)
      where split = '$' : takeWhile (/= '$') rest ++ "$"
    splitString s = split : splitString (drop (length split) s)
      where split = takeWhile (/= ' ') s

toSrc :: String -> String 
toSrc  = takeWhile (/= '$') . drop 1 

toSrcName :: String -> String
toSrcName = drop 1

toFp :: Int -> String -> FilePath 
toFp n = drop n 

toQuotFp :: Int -> String -> FilePath 
toQuotFp n = takeWhile (/= '\"') . drop (n + 1) 

toInt :: String -> Int
toInt  = foldl (\n c -> 10 * n + digitToInt c) 0

}