
module Utils
  ( Padme(..)         -- Padded list datatype.
  , (.*)              -- Function comp. with two args.
  , allEq             -- Check if list elements are all equal.
  , combinations      -- Combine elements of two lists in all ways.
  , concatMapM        -- ConcatMap for monadic operations.
  , deggar            -- Pad list of strings to same length using whitespace.
  , deleteAtIdx       -- Delete an element of a list by index.
  , divide            -- Divide a list in two sub-lists in all possible ways.
  , eitherHead        -- Safe head with error message. 
  , groupOthers       -- Group other elements in a list aside curr. index.
  , names             -- Infinite list of variable names for alpha-renaming.
  , noDupes           -- Check a list has no duplicates.
  , notNull           -- not . null
  , powerset          -- Calculate the powerset in list form.
  , prepStr           -- stripSpace + toLower
  , replaceAtIdx      -- Replace an element of a list by index.
  , safeHead          -- Head using Maybe.
  , safeTail          -- Tail using Maybe.
  , safeTailSnocPath  -- Tail of a SnocPath using Maybe
  , singleton         -- Check if a list is a singleton.
  , stripEndSpace     -- Remove trailing whitespace.
  , stripSpace        -- Remove surround whitespace.
  , stripStartSpace   -- Remove leading whitespace.
  ) where

import Control.Monad (filterM, replicateM)
import Data.Char     (isSpace, toLower)
import Data.List     ( dropWhileEnd, inits, permutations
                     , tails, transpose )

import Language.KURE (SnocPath(..))

{-
  Information:
  -----------------------------------------------------------------------------
  - General helper functions for all modules.
-}

-- Padding lists: -------------------------------------------------------------
-- Interesting solution by McBride: https://stackoverflow.com/questions/
--   21349408/zip-with-default-value-instead-of-dropping-values

data Padme m = (:-) { padded :: [m], padder :: m } deriving (Show, Eq)

instance Functor Padme where 
  fmap = (<*>) . pure

instance Applicative Padme where
  pure                    = ([] :-)
  (fs :- f) <*> (ss :- s) = zapp fs ss :- f s 
                            where
                              zapp  []        ss       = map f ss
                              zapp  fs        []       = map ($ s) fs
                              zapp  (f : fs)  (s : ss) = f s : zapp fs ss

-- Pad a list of strings to the same length;
-- Name was given by McBride, sounds funny so I left it.
deggar :: [String] -> [String]
deggar  = transpose . padded . traverse (:- ' ')

--- Other helpers: ------------------------------------------------------------

-- Infinite list of fresh variable names to be used for alpha-renaming.
names :: [String]
names  = [1..] >>= flip replicateM ['a'..'z']

-- Check if all list elements are unique.
noDupes :: Eq a => [a] -> Bool 
noDupes  = flip go []
            where
             go []       _  = True
             go (x : xs) ys = x `notElem` ys && go xs (x : ys)

-- Generate all ways of matching elements in two lists
combinations :: [a] -> [a] -> [[(a, a)]]
combinations xs ys 
  | length xs /= length ys = error "shouldn't happen: combinations"
  | otherwise              = fmap (zip xs) (permutations ys)

-- Check if all elements in a list are equal.
allEq :: Eq a => [a] -> Bool
allEq xs = all (== head xs) (tail xs) 

-- Divide a list in two sub-lists in all possible ways.
divide :: Eq a => [a] -> [([a], [a])]
divide xs = zip (powerset xs) (powerset' xs)
            where powerset' = filterM (const [False, True])

-- Replace an element of a list by index.
replaceAtIdx :: Int -> a -> [a] -> [a]
replaceAtIdx _   _   []     = error "index too large: replaceAtIdx"
replaceAtIdx 0   x (_ : ys) = x : ys 
replaceAtIdx idx x (y : ys) = y : replaceAtIdx (idx - 1) x ys

-- Delete an element by index.
deleteAtIdx :: Int -> [a] -> Maybe [a]
deleteAtIdx i _  | i < 0 = Nothing
deleteAtIdx i xs = case r of 
                     []     -> Nothing 
                     _ : r' -> Just (l ++ r')
                   where (l, r) = splitAt i xs

-- E.g., [1,2,3] -> [[2,3], [1,3], [1,2]]
groupOthers :: [a] -> [[a]]
groupOthers xs = init . fmap (\(us, ws) -> us ++ drop 1 ws) 
                  $ zip (inits xs) (tails xs)    

-- I use this for function composition with two arguments.
infixr 8 .*
(.*) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.*)  = fmap . fmap 

-- ConcatMap for monadic actions.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

powerset :: [a] -> [[a]]
powerset  = filterM (const [True, False])

notNull :: [a] -> Bool 
notNull  = not . null 

singleton :: [a] -> Bool 
singleton [_] = True 
singleton _   = False

safeHead :: [a] -> Maybe a
safeHead []  = Nothing
safeHead xs  = Just (head xs)

eitherHead :: String -> [a] -> Either String a
eitherHead s []      = Left s
eitherHead _ (x : _) = Right x  

safeTail :: [a] -> [a]
safeTail []  = []
safeTail xs  = tail xs

safeTailSnocPath :: SnocPath c -> SnocPath c
safeTailSnocPath (SnocPath [])       = SnocPath []
safeTailSnocPath (SnocPath (_ : cs)) = SnocPath cs 

-- Basic string operations: ---------------------------------------------------

stripStartSpace :: String -> String 
stripStartSpace  = dropWhile isSpace

stripEndSpace :: String -> String 
stripEndSpace  = dropWhileEnd isSpace

stripSpace :: String -> String 
stripSpace  = stripStartSpace . stripEndSpace

prepStr :: String -> String 
prepStr  = stripSpace . fmap toLower