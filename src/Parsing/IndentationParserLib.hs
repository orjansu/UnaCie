
module IndentationParserLib

  ( Parser      -- Type of parsers.
  , runParser   -- Top-level runner.
  , isSymChar   -- Check if a character is symbolic.
  , parse       -- Used to test parsers.

    -- Parser combinators: -- 

  , (+++)
  , bracket
  , chainl0
  , chainl1
  , chainr0
  , chainr1
  , choice
  , many0
  , many1
  , manyOffside0
  , manyOffside1
  , sat
  , sepby0
  , sepby1

    -- Useful parsers: -- 

  , alpha 
  , alphaNum 
  , char 
  , digit 
  , ident 
  , infixSymIdent 
  , int 
  , lower 
  , nat 
  , print 
  , printNoSpeech 
  , string 
  , symChar 
  , symIdent 
  , uIdent 
  , upper 

     -- Token parsers: -- 

  , identifier 
  , integer 
  , natural 
  , pChar 
  , str 
  , sym
  , symIdentifier 
  , uIdentifier

    -- Symbol parsers: -- 

  , arr_      
  , case_     
  , coloneq_    
  , colons_   
  , comma_      
  , comment_    
  , dot_       
  , dots_       
  , emptyhole_  
  , eq_        
  , fwdslash_ 
  , ifCons_
  , in_         
  , langle_     
  , lbrace_   
  , lbrack_   
  , let_        
  , lparen_    
  , mid_
  , nil_        
  , of_        
  , parr_       
  , pfCons_    
  , plambda_  
  , ptick_      
  , quote_      
  , rangle_    
  , rbrace_     
  , rbrack_    
  , rparen_    
  , semicolon_  
  , speech_     
  , tick_      
  , underscore_
  , wildcard_ 

    -- Misc: -- 
    
  , junk
   
  ) where 

import Prelude hiding             (abs, print)
import Data.Char                  ( isAlpha, isAlphaNum, isDigit, isAsciiLower
                                  , isPrint, isSpace, isAsciiUpper )
import Control.Monad.Reader       (ReaderT, ask, local, runReaderT)
import Control.Monad.State.Strict ( MonadState, StateT(..), get, guard
                                  , modify, mplus, mzero, put, void )
                

{-
   Information:
   ------------
   - Library for indentation parsing, based on Hutton's paper. See 
     http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf for details, deferred 
     here;
   - We're not concerned with parser error messages for the time being. So 
     /any/ parse error is on the user to sort out. In practice, this isn't
     too much of an issue because the definitions we work with are simple
     (for now at least).
-}

type Parser a = ReaderT Pos (StateT Pstring []) a
type Pstring  = (Pos, String)
type Pos      = (Int, Int)

-- Runners: -------------------------------------------------------------------                                         

runParser :: Parser a -> String -> Maybe a 
runParser p inp = case parse (junk >> p) inp of
                    ((res, (_, "")) : _) -> Just res 
                    _  -> Nothing                                

-- Parsing functions: ---------------------------------------------------------                               
                                   
parse :: Parser a -> String -> [(a, Pstring)]
parse p inp = runStateT (runReaderT p (1, -1)) ((1, 1), inp)

parse' :: Parser a -> Pos -> Pstring -> [(a, Pstring)]
parse' p pos = runStateT (runReaderT p pos) 

update :: MonadState s m => (s -> s) -> m s
update f = get >>= \st -> modify f >> return st 

newState :: Pstring -> Pstring
newState ((l, c), x : xs) = (newpos, xs)
  where newpos = case x of
                  '\n' -> (l + 1, 1)
                  '\t' -> (l, ((c `div` 4) + 1) * 4)
                  _    -> (l, c + 1)                                                 
newState ps =  ps -- In practice, this /shouldn't/ occur.                       

onside :: Pos -> Pos -> Bool 
onside (l, c) (dl, dc) = c > dc || l == dl

force :: Parser a -> Parser a
force p = do 
            defPos <- ask
            st     <- get
            case parse' p defPos st of 
              ((x, st') : _) -> put st' >> return x
              _              -> mzero -- In practice, this /shouldn't/ occur.

item :: Parser Char 
item  = do 
          defPos       <- ask 
          (pos, x : _) <- update newState
          guard (onside pos defPos) 
          return x

first :: Parser a -> Parser a
first p = do 
            defPos <- ask
            st     <- get
            case parse' p defPos st of
              []            -> mzero 
              ((x, st) : _) -> put st >> return x

----- Derived combinators: ----------------------------------------------------

(+++) :: Parser a -> Parser a -> Parser a 
p +++ q = first (p `mplus` q)
                                   
sat :: (Char -> Bool) -> Parser Char 
sat p = do 
          x <- item 
          x <$ guard (p x) 

many0 :: Parser a -> Parser [a]
many0 p = force (many1 p +++ return [])

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many0 p

sepby0 :: Parser a -> Parser b -> Parser [a]
p `sepby0` sep = p `sepby1` sep +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = (:) <$> p <*> many0 (sep >> p)           

chainl0 :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl0 p op a = p `chainl1` op +++ return a
       
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest x = (do
                    f <- op
                    y <- p
                    rest $ f x y)
                    +++ return x

chainr0 :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr0 p op a = p `chainr1` op +++ return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = p >>= rest 
  where rest x = (do
                    f <- op 
                    y <- p `chainr1` op 
                    return $ f x y) 
                    +++ return x

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = open *> p <* close

choice :: [Parser a] -> Parser a
choice  = foldr (+++) mzero   

----- Useful parsers: ---------------------------------------------------------

char :: Char -> Parser Char
char  = sat . (==)

symChar :: Parser Char 
symChar  = sat isSymChar
                        
digit :: Parser Char 
digit  = sat isDigit

alpha :: Parser Char
alpha  = sat isAlpha

upper :: Parser Char                 
upper  = sat isAsciiUpper

lower :: Parser Char
lower  = sat isAsciiLower

alphaNum :: Parser Char
alphaNum  = sat isAlphaNum

print :: Parser Char 
print  = sat isPrint

printNoSpeech :: Parser Char 
printNoSpeech  = sat (\c -> c /= '\"' && isPrint c)

nat :: Parser Int 
nat  = read <$> many1 digit

int :: Parser Int 
int  = neg +++ nat  
       where neg = char '-' >> negate <$> nat

string :: String -> Parser String
string ""       = return ""
string (c : cs) = char c >> string cs >> return (c : cs)

ident :: Parser String 
ident  = (:) <$> lower <*> many0 alphaNum

uIdent :: Parser String 
uIdent  = (:) <$> upper <*> many0 alphaNum

-- Prefix: ($+) (for symChar $).
symIdent :: Parser String
symIdent  = (\s -> '(' : s ++ ")") <$> bracket lparen_ (many1 symChar) rparen_ 

-- Infix: $+ (for symChar $).
infixSymIdent :: Parser String
infixSymIdent  = many1 symChar

----- Lexical combinators: ----------------------------------------------------

space :: Parser ()
space  = void $ sat isSpace

comment :: Parser () 
comment  = void (comment_ >> many0 (sat (/= '\n')))

junk :: Parser ()
junk  = local (const (1, -1)) . void $ many0 (space +++ comment)

token :: Parser a -> Parser a 
token  = (<* junk)

off :: Parser a -> Parser a 
off p = do 
          (_, dc)     <- ask 
          ((l, c), _) <- get 
          guard (c == dc)
          local (const (l, dc)) p

manyOffside0 :: Parser a -> Parser [a]
manyOffside0 p = manyOffside1 p +++ return []

manyOffside1 ::  Parser a -> Parser [a]
manyOffside1 p = get >>= \(pos, _) -> local (const pos) (many1 $ off p)

----- Token parsers: ----------------------------------------------------------

natural :: Parser Int 
natural  = token nat 
 
integer :: Parser Int 
integer  = token int 

str :: Parser String 
str  = token (bracket speech_ (many0 printNoSpeech) speech_)

sym :: String -> Parser String
sym  = token . string 

pChar :: Char -> Parser String 
pChar c  = token $ return <$> char c

-- lower alphanum*
identifier :: Parser String 
identifier  = token ident

-- upper alphanum*
uIdentifier :: Parser String 
uIdentifier  = token uIdent

-- Symbol identifiers e.g., (++) xs ys
-- True for infix too e.g., xs ++ ys
symIdentifier :: Bool -> Parser String 
symIdentifier True  = token (symIdent +++ infixSymIdent)
symIdentifier False = token symIdent

----- Symbol parsers: ---------------------------------------------------------

let_, in_, case_, of_     ::  Parser String
arr_, eq_, fwdslash_      ::  Parser String 
speech_, dot_, dots_      ::  Parser String
comma_, colons_, pfCons_  ::  Parser String
coloneq_, tick_, lparen_  ::  Parser String
rparen_, lbrack_, rbrack_ ::  Parser String
lbrace_, rbrace_          ::  Parser String
wildcard_, underscore_    ::  Parser String 
emptyhole_, nil_          ::  Parser String
comment_, semicolon_      ::  Parser String
mid_, quote_, rangle_     ::  Parser String
langle_ ,ptick_, plambda_ ::  Parser String
parr_, ifCons_            ::  Parser String

let_        = sym "let"
in_         = sym "in"
case_       = sym "case"
of_         = sym "of"
arr_        = sym "->"
eq_         = sym "="
fwdslash_   = sym "\\"
speech_     = sym "\""
dot_        = sym "."
dots_       = sym ".."
comma_      = sym ","
colons_     = sym "::"
ifCons_     = sym ":"
pfCons_     = sym "(:)"
coloneq_    = sym ":="
tick_       = sym "`"
lparen_     = sym "("
rparen_     = sym ")"
lbrack_     = sym "["
rbrack_     = sym "]"
lbrace_     = sym "{"
rbrace_     = sym "}"
langle_     = sym "<"
rangle_     = sym ">"
wildcard_   = sym "_"
underscore_ = sym "_"
emptyhole_  = sym "[-]"
nil_        = sym "[]"
-- ## Don't use sym because sym uses junk. ##
comment_    = string "--"
semicolon_  = sym ";"
mid_        = sym "|"
quote_      = sym "\'"
ptick_      = pChar '\10004'
plambda_    = pChar '\955'
parr_       = pChar '\10142'

-- Helpers: -------------------------------------------------------------------

-- Valid symbolic characters, to be used in symbolic variable names
isSymChar :: Char -> Bool 
isSymChar c = c >= '!'
                && c <= '@' 
                && c `notElem` "()$0123456789"