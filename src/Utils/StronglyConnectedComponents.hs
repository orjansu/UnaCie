{-# LANGUAGE TupleSections #-}

module StronglyConnectedComponents
 ( hasSCCFromEdges  -- Construct a graph from a list of pairs (representing 
                    -- edges) and see check to see if it has SCC.
 , hasSCCFromEdges' -- As above but takes an adjacency list as input.
 ) where 
  
import Data.Graph     (SCC(..), buildG)
import Data.Graph.SCC (sccList)
import Data.List      (elemIndex, union)
import Data.Maybe     (fromJust)

{-
  Information:
  -----------------------------------------------------------------------------
  - We are often interested in strongly connected components of directed 
    graphs for dependency checking. E.g., checking for loops in script imports 
    and checking whether bindings are (mutually) recursive;
  - Here we implement a SCC test by first converting a directed adjacency list
    to a directed graph via an indexing process and then testing for SCC using 
    the Data.Graph.SCC package.
-}

-- Returns a list of directed adjacency pairs labelled by indexed vertices, 
-- and the lower and upper bounds of the indices used
idxVerts :: Eq a => [(a, a)] -> ([(Int, Int)], Int, Int)
idxVerts xs = ( fmap (\(x, y) -> ( fromJust $ elemIndex x verts
                                 , fromJust $ elemIndex y verts )) xs
              , 0
              , length verts - 1 )
              where 
               verts = foldr (\(x, y) vs -> vs `union` [x, y]) [] xs

-- Takes a list of directed adjacency pairs and returns whether they 
-- form a directed graph that has any strongly connected components
hasSCCFromEdges :: Eq a => [(a, a)] -> Bool 
hasSCCFromEdges xs = any isCyclic (sccList graph)
                     where 
                      (verts, lBound, uBound) = idxVerts xs 
                      graph = buildG (lBound, uBound) verts
                     
                      isCyclic CyclicSCC{} = True
                      isCyclic _           = False

-- As above but takes a more general adjacency list representation
-- ("a", ["b", "c"]) => { a -> b, a -> c }
hasSCCFromEdges' :: Eq a => [(a, [a])] -> Bool 
hasSCCFromEdges'  = hasSCCFromEdges . concatMap (\(x, ys) -> fmap (x,) ys)