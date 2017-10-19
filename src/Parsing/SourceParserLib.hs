
module SourceParserLib
  ( abs           -- Source parser for lambda abstractions.
  , absBindName   -- Parser for lambda binders.
  , alt           -- Source parser for case alts.
  , bind          -- Source parser for let binds.
  , bindName      -- Parser for binders.
  , cBindName     -- Parser for CBind binders.
  , esac          -- Source parser for case statements.
  , listVar       -- Parser for variable lists.
  , litInt        -- Source parser for integer literals.
  , litStr        -- Source parser for string literals.
  , nonSymVar     -- Source parser for non-symbolic variables.
  , nonSymVarName -- Parser for non-symbolic variables.
  , reserved      -- List of reserved names.
  , tBindName     -- Parser for TBind binders.
  , tel           -- Source parser for let statements.
  , tick          -- Source parser for ticks statements.
  , var           -- Source parser for variables.
  , varName       -- Parser for variable names (symbolic and non-symbolic).
  ) where 

import CtxAST (Name, Con(..))               
import Utils (noDupes)
import IndentationParserLib  
  ( Parser
  , (+++)
  , arr_
  , bracket
  , case_
  , choice
  , colons_
  , dot_
  , eq_
  , fwdslash_
  , identifier
  , in_
  , integer
  , lbrace_
  , lbrack_
  , let_
  , lparen_
  , manyOffside1
  , nil_
  , of_
  , parr_
  , pfCons_
  , plambda_
  , ptick_
  , rbrace_
  , rbrack_
  , rparen_
  , semicolon_
  , sepby1
  , str
  , symIdentifier
  , tick_
  , uIdentifier
  , wildcard_ 
  )

import Prelude hiding (abs)
import Control.Monad  (guard)

{-
  <TO-DO>:

  Information:
  -----------------------------------------------------------------------------
  - Parsing contexts and context patterns has a lot of overlap, here we
    abstract out as much of that as possible;
  - See context pattern matching for more information on context patterns.
-}

-------------------------------------------------------------------------------
-- Reserved words: --
-------------------------------------------------------------------------------

reserved :: [String]
reserved  = [ "let", "in", "case", "of", "->", ",", "[", "]" 
            ,  "[]", "::", ".", "=", "'", "`", ":=", "..", ":" 
            ,  "_", "[-]", "(::)", ";"
            ]

{-- Names that aren't reserved: -----------------------------------------------

  We use the following naming conventions:
   (1) 'Standard' variable names are alphanumeric and must start with a 
       lower-case alpha;
   (2) 'Symbol' variable names contain characters between '!' and '@' 
       (inclusive) and may /not/ contain parentheses. Binding occurrences
       e.g., let (++) = ... /must/ be surrounded with parentheses. Applied
       occurrences e.g., xs ++ ys, /may/ be infix (if they relate to a binary 
       operator), in which case the parentheses are dropped or prefix 
       (++) xs ys in which case the parentheses are kept;
   (3) TBind names <name> = <term> are either standard or symbol. CBind 
       names <kind>_<name> = <ctx> are alphanumeric and must start with an 
       upper-case alpha;
   (4) Let binding names let (++) = .. are either standard or symbol;
   (5) Abstracted variables names \x. are standard only;
   (6) Pattern matching variable names (x::xs) are standard only;
   (7) List variable names [a,b,c] are standard only.
-}    

-- Vars (true for /infix/ symbol identifiers).
varName :: Parser Name 
varName  = identifier +++ symIdentifier True >>= notReserved

-- Vars (non-symbol).
nonSymVarName :: Parser Name 
nonSymVarName  = identifier >>= notReserved

-- Abs.
absBindName :: Parser Name 
absBindName  = identifier >>= notReserved

-- TBinds.
tBindName :: Parser Name
tBindName  = identifier +++ symIdentifier False >>= notReserved

-- CBinds.
cBindName :: Parser Name
cBindName  = uIdentifier >>= notReserved

-- Binds.
bindName :: Parser Name
bindName  = identifier +++ symIdentifier False >>= notReserved

-- Helper.
notReserved :: String -> Parser String 
notReserved s = s <$ guard (s `notElem` reserved)

-------------------------------------------------------------------------------
-- Generic source language parsers: --
-------------------------------------------------------------------------------
-- These parse the /terminals/ of the source language.

var :: (Name -> a) -> Parser a 
var  = (<$> varName)

nonSymVar :: (Name -> a) -> Parser a 
nonSymVar  = (<$> nonSymVarName)

litInt :: (Int -> a) -> Parser a 
litInt  = (<$> integer)

litStr :: (String -> a) -> Parser a 
litStr  = (<$> str)

abs :: (Name -> b -> a) -> Parser b -> Parser a 
abs f p = f <$> (fwdslash_ +++ plambda_ *> absBindName) <*> (dot_ *> p)

tick :: (a -> a) -> Parser a -> Parser a
tick f p = f <$> (tick_ +++ ptick_ *> p)

tel :: ([b] -> c -> a) -> Parser b -> Parser c -> Parser a 
tel f p1 p2  =  
  -- Indented
  (f <$> (let_ *> manyOffside1 p1) <*> (in_ *> p2)) +++ 
  -- Inline using { .. ; .. }
  (f <$> (let_ *> bracket lbrace_ (p1 `sepby1` semicolon_) rbrace_) <*> (in_ *> p2))
                   
bind ::  (Name -> a -> b) -> Parser a -> Parser b 
bind f p = f <$> bindName <*> (eq_ *> p)

esac :: (b -> [c] -> a) -> Parser b -> Parser c -> Parser a 
esac f p1 p2  = 
  -- Indented:
  (f <$> (case_ *> p1) <*> (of_ *> manyOffside1 p2)) +++ 
  -- Inline using { .. ; .. }:
  (f <$> (case_ *> p1) <*> (of_ *> bracket lbrace_ (p2 `sepby1` semicolon_) rbrace_))

alt :: (Con -> [Name] -> a -> b) -> Parser a -> Parser b
alt f p = do 
            (con, nss) <- patterns
            -- Ensure binding names are /unique/
            guard (noDupes nss)
            f con nss <$> (arr_ +++ parr_ *> p)
  where 

       -- Pattern matching 'patterns'. Note that we only allow pattern 
       -- matching on lists of /variables/. E.g., CONS [x,xs] is valid for 
       -- pattern matching, but CONS [1,_] isn't.
       patterns :: Parser (Con, [Name])
       patterns  = choice [list, var, tluafed, litint, litword]
                    where
                      -- Lists
                      list    = listVar >>= \nss -> 
                                if null nss 
                                   then return (NIL, [])
                                   else return (CONS, nss)
                      -- Built-in patterns
                      var     = (\ns -> (VARIABLE, [ns])) <$> nonSymVarName
                      litint  = (\i -> (LITINT i, [])) <$> integer
                      litword = (\s -> (LITSTR s, [])) <$> str
                      -- Default pattern
                      tluafed = const (DEFAULT, []) <$> wildcard_ 

-- Lists of variables.
listVar :: Parser [Name]
listVar  = choice [ pfCons     -- Prefix CONS
                  , ifCons     -- Infix CONS
                  , singleton  -- CONS [_,NIL]
                  , nil        -- NIL
                  ]
  where 
    -- (::) x xs
    pfCons    = sequence [pfCons_ *> nonSymVarName, nonSymVarName] 
    -- x::xs/(x::xs)
    ifCons    = sequence [nonSymVarName <* colons_, nonSymVarName] 
                +++ bracket lparen_ (sequence [nonSymVarName 
                        <* colons_, nonSymVarName]) rparen_
    -- [x]
    singleton = return <$> bracket lbrack_ nonSymVarName rbrack_
    -- [] 
    nil       = const [] <$> nil_ 