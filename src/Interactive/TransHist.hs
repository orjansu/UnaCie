
module TransHist
  ( TransHist(..)   -- Storing the transformation history.
  , delHist         -- Delete transformation history.
  , emptyTransHist  -- Empty history.
  , initTransHist   -- Initialise a transformation history.
  , maxHistIndex    -- Size of the transformation history.
  ) where 

import CmdAST    (Cmd)
import CmdHist   (CmdHist, emptyCmdHist)
import Crumb     (Crumb)
import CtxAST    (Term)
import Relations (Relation)

import Language.KURE (AbsolutePath)

{-
  <TO-DO>: Transformation history output using paths.

  Information:
  -----------------------------------------------------------------------------
  (1) ** WHEN THEN GLOBAL RELATION IS SET **
      
    - The transformation history is effectively the proof state history plus
      each command that has been executed taking the interpreter from one
      /proof/ state to the next;
    - The transformation history can be seen as a series of semi-formal
      steps of inequational reasoning of the form:

       t
         REL_i { cmd1 }
       t'
         REL_j { cmd2 }
       t''

      where t'{^n} are the proof states and REL_k { cmdX } are the
      proof hints. Here t'{^n} are stored in stateHist and REL_k { cmdX } 
      are stored in cmdHist
      
  (2) ** WHEN THE GLOBAL RELATION IS NOT SET **

    - The transformation history simply records the term before and after
      a transformation has been executed, and tracks which command
      was responsible for the transformation and its corresponding relation;
    - Important: in this instance, the transformation history does /not/
      necessarily constitute semi-formal steps of inequational reasoning, 
      but in theory /could/ if the user was careful with their inequational 
      relations.

  - The transformation history can be reviewed/exported by the user at any time
  -- A warning will be printed if the history is exported when a global 
     relation is not set.
  - We store paths in stateHist in order to provide a transformation history 
    that reflects what the user could see on the command line in each proof 
    state, as navigation commands allow us to move through a term's 
    AST in order to apply transformation to specific sub-term.
-}

data TransHist = 
  TransHist { stateHist :: [(Term, AbsolutePath Crumb)]
            , cmdHist   :: CmdHist 
            } deriving Eq

-- Initial settings: ----------------------------------------------------------

emptyTransHist :: TransHist
emptyTransHist  = TransHist { stateHist = []
                            , cmdHist = emptyCmdHist 
                            }

-- The transformation history begins with the transformation command
-- invoked by the user e.g.., trans $\x.x$.
initTransHist :: Term -> Cmd -> Maybe Relation -> TransHist
initTransHist term cmd mrel = TransHist { stateHist = [(term, mempty)]
                                        , cmdHist   = [(cmd, mrel)] 
                                        }

-- Helpers: -------------------------------------------------------------------

maxHistIndex :: TransHist -> Int 
maxHistIndex  = pred . length . stateHist

-- Delete history up to index i.
delHist :: Int -> TransHist -> TransHist
delHist i h = TransHist { stateHist = take (l - i) (stateHist h)
                        , cmdHist   = take (l - i) (cmdHist h) }
              where l = maxHistIndex h