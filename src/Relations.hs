
module Relations
  ( Relation(..)  -- Datatype to capture relations.
  , impl          -- Check if one relation implies another according to 
                  -- defined relationships in inclusions.
  , strToRel      -- Convert a string to a relation.
  , relToStr      -- Convert a relation to a string.
  ) where

import Utils (prepStr)

{-
  <TO-DO>: - Can we update the relations to their actual symbols from the
             paper? Looks like we could with our own font (e.g., via Inkscape) 
             but not sure how practical that is.

  Information:
  -----------------------------------------------------------------------------
  - The improvement relations currently supported, and the relationships
    between them.
-}

data Relation = R_WI           -- Weak improvement
              | R_WCE          -- Weak cost-equivalence
              | R_I            -- Improvement
              | R_CE           -- Cost-equivalence
              | R_EQ           -- Definitional equality (meta-level)
              | R_UNDEFINED    -- Undefined relation
                deriving Eq

-- Show instance of each relation is the best approximation of its 
-- actual symbol using existing unicode math symbol(s).
instance Show Relation where
  show R_WI        = "\10886"           -- (GT or approx.)
  show R_WCE       = "\10885\10886"     -- (LT or approx., GT or approx.)
  show R_I         = "\8819"            -- (GT or equiv. to)
  show R_CE        = "\8818\8819"       -- (LT or equiv. to, GT or equiv. to)
  show R_EQ        = "\8801"            -- (Identical to)
  show R_UNDEFINED = "\8709"            -- (Empty set)

{-
  - We have the following relationships, taken from the lattice diagram 
    in the paper;
  - See Relations.agda in the Doc folder for a formalisation.

  --------------------------------------------------------------------
       Relation:         |   =>:
  -----------------------|---------------------------------------------
   Weak improvement      |   Weak improvement
   Weak cost-equivalence |   Weak cost-equivalence, weak improvement
   Improvement           |   Weak improvement, improvement
   Cost-equivalence      |   Weak cost-equivalence, cost-equivalence,
                         |   weak improvement, improvement
  -------------------------------------------------------------------
-}
inclusions :: [(Relation, [Relation])]
inclusions  = [ (R_WI, [R_WI])
              , (R_WCE, [R_WI, R_WCE])
              , (R_I, [R_WI, R_I])
              , (R_CE, [R_WI, R_WCE, R_I, R_CE])
              , (R_EQ, [R_WI, R_WCE, R_I, R_CE, R_EQ]) -- Meta-level
              , (R_UNDEFINED, [])
              ]

-- Helpers: -------------------------------------------------------------------

-- Check if r1 -> r2 according to inclusions.
impl :: Relation -> Relation -> Bool
impl r1 r2 = maybe False (r2 `elem`) (lookup r1 inclusions)

-- Converting between strings and relations: --

-- For parsing relations input on the command line as command parameters.
strToRel :: String -> Maybe Relation
strToRel s = case prepStr s of -- prepStr trims whitespace and lowers.
  "wi"  -> Just R_WI
  "wce" -> Just R_WCE
  "i"   -> Just R_I
  "ce"  -> Just R_CE
  "eq"  -> Just R_EQ
  "u"   -> Just R_UNDEFINED
  _     -> Nothing

-- For pretty printing.
relToStr :: Relation -> String
relToStr R_WI        = "WI"
relToStr R_WCE       = "WCE"
relToStr R_I         = "I"
relToStr R_CE        = "CE"
relToStr R_EQ        = "EQ"
relToStr R_UNDEFINED = "UNDEFINED"