
module PPLib
  ( Colour          -- Colours are just strings.
  , Outputable(..)  -- Anything being pretty printed is Outputable.
  , unlines'        -- unlines without the trailing '\n'
  , highlightError  -- Highlighting error messages red
  , highlightCritError -- As above, but for critical errors
  , indentHighlightError

    -- Terminal symbols: -- 
  , arr    
  , coloneq  
  , colons   
  , dot      
  , emptyHole 
  , eq        
  , esac    
  , fo  
  , ifCons     
  , lambda    
  , mid        
  , newline  
  , ni        
  , nil      
  , strArr 
  , tel      
  , tick     
  , underscore 
  , wildcard 

  -- Error symbols: --

  , invalidC      -- Invalid constructor.
  , invalidDT     -- Invalid datatype.
  , miss          -- Missing keyword.

  , (<<+>>)       -- Place two docs side by side (horizontally) 
  , bySide        -- Generalised above, can add vertical sep.
  , colourLookup  -- ANSI escape code lookup for colours.
  , colours       -- Show all ANSI escape colours.
  , genStyle      -- Generate a style based on a width.
  , isInfix       -- Check if an operator can be displayed infix.
  , squashString  -- Display a string for a given line width.
  , toInfix       -- Convert an operator to infix, i.e., drop outer parens.
  ) where 

import CtxAST (Name)
import Utils  (Padme(..), deggar, prepStr)

import Data.List (intercalate)
import Text.PrettyPrint.HughesPJ 
 ( Doc
 , Mode(..)
 , Style(..)
 , char
 , fsep
 , lineLength
 , mode
 , render
 , renderStyle
 , ribbonsPerLine
 , text 
 )

{-
   Information:
   ----------------------------------------------------------------------------
   - Pretty printing functions used by CtxPP, CtxPatPP and CtxShowAST.
-}

-- ANSI colour escape codes are just strings.
type Colour = String 

-- Anything that gets pretty printed is /Outputable/.
class Outputable a where
  ppr :: a -> Doc 

-- Check if a variable name is an infix operator;
-- This doesn't have to be any more sophisticated because 
-- parsers check for valid infix anyway.
isInfix ::  Name -> Bool 
isInfix ('(' : ns) = dropWhile (/= ')') ns == ")"
isInfix _ = False

-- Convert a prefix symbolic name to infix (assumes isInfix).
toInfix :: Name -> Doc 
toInfix  = text . takeWhile (/= ')') . drop 1

-- Terminal symbols: ----------------------------------------------------------

lambda, dot, tick, emptyHole      :: Doc 
tel, eq, coloneq, ni, esac        :: Doc 
fo, colons, nil, arr, newline     :: Doc 
mid, wildcard, underscore, strArr :: Doc
miss, invalidDT, invalidC, ifCons :: Doc

lambda     = char '\955'
dot        = char '.'
tick       = char '\10004'
emptyHole  = text "[-]"
tel        = text "let"
eq         = char '='
coloneq    = text ":="
ni         = text "in"
esac       = text "case"
fo         = text "of"
colons     = text "::"
ifCons     = text " : "
nil        = text "[]"
arr        = char '\10142'
newline    = char '\n'
mid        = char '|'
wildcard   = char '_' 
underscore = char '_'
strArr     = char '\8655'
miss       = text "\ESC[31m## missing keyword ##\ESC[m"
invalidDT  = text "\ESC[31m## unrecognised datatype ##\ESC[m"
invalidC   = text "\ESC[31m## unrecognised constructor ##\ESC[m"

-- ANSI colours: --------------------------------------------------------------
-- So we can add colour when we dynamically stylise ASTs, this is still being
-- implemented.

-- Need to actually add some colours here;
-- Could use an index instead of a string?
colourLookup :: String -> Colour 
colourLookup s = case (prepStr s) of 
  "red" -> "\ESC[38;5;124m"
  _     -> ""

-- General helpers: -----------------------------------------------------------

-- Put a list of multi-line documents side by side: output as String;
-- We provide a separater that vertically separates each doc.
bySide :: [Doc] -> String -> String
bySide [] _   = ""
bySide xs sep = unlines 
                 . foldr1 side 
                 . fmap (deggar . lines . render) 
                 $ xs
   where 
        side s1 s2   = fmap (intercalate sep) . padded $
                        padLeft s1 (pad s1) <*> padRight s2 (pad s2)
        pad s        = replicate (length $ head s) ' '
        padLeft  s p = (:) <$> s :- p
        padRight s p = (:) <$> s :- p <*> pure []

-- Binary version of bySide with whitespace sep.
(<<+>>) :: Doc -> Doc -> String 
s1 <<+>> s2 = bySide [s1,s2] " "

-- Display a string for a given line width.
squashString :: Int -> String -> [String]
squashString width = lines
                      . renderStyle (genStyle width) 
                      . fsep 
                      . fmap text 
                      . words

-- Generate a style based on line length.
genStyle :: Int -> Style
genStyle len = Style { mode           = PageMode
                     , lineLength     = len
                     , ribbonsPerLine = 1 :: Float }

-- Show all ANSI foreground colours we can use.
colours :: IO ()
colours  = mapM_ (\i -> do 
             putStr (show i)
             putStrLn ("= \ESC[38;5;" ++ show i ++ "mTEST\ESC[m")) 
           [0..255 :: Int]

-- Doesn't have the trailining '\n'
unlines' :: [String] -> String
unlines' = reverse . drop 1 . reverse . unlines 

-- Highlight's an error message red that spans multiple lines.
highlightCritError :: [String] -> [String]
highlightCritError = fmap (\s -> "\ESC[30m\ESC[1m\ESC[48;5;160m " ++ s ++ " \ESC[m")

-- Highlight's an error message red that spans multiple lines.
highlightError :: [String] -> [String]
highlightError = fmap (\s -> "\ESC[37m\ESC[41m " ++ s ++ " \ESC[m")

-- Highlight's an error message red that spans multiple lines.
indentHighlightError :: String -> String
indentHighlightError = unlines' . fmap (\s -> "\ESC[37m\ESC[41m " ++ s ++ " \ESC[m")  . lines
