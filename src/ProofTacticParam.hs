
module ProofTacticParam 
  ( PBinding(..)   -- Datatype for proof tactic parameters.
  ) where

import CtxAST  (Ctx, Name, Term)
import CtxKind (CtxKind)

{-
  <TO-DO>: - This is just a concept currently.

  Information:
  -----------------------------------------------------------------------------
  - We often need to instantiate some of the parameters of a proof tactic 
    (e.g., improvement induction). We do this with via parameter bindings;
  - PBindings are typically defined externally by the user in source files and
    imported into the system, though they can be defined on the command line 
    if needed.
-}
data PBinding = CParam Name CtxKind Ctx   -- <kind>_<param> := <ctx>
              | TParam Name Term          -- <param> := <term>
                deriving Eq