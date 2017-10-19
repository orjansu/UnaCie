
module CtxAST
  ( Alt(..)            -- Case alternatives.
  , Bind(..)           -- Let bindings.
  , Con(..)            -- Constructors.
  , Ctx(..)            -- Contexts.
  , GBind(..)          -- Global bindings.
  , Idx                -- Indices for let bindings/case alternatives 
                       -- (for navigation).
  , L_Alt(..)          -- Labelled alternatives.
  , L_Bind(..)         -- Labelled let bindings.
  , L_Ctx(..)          -- Labelled contexts.
  , L_GBind(..)        -- Labelled global bindings.
  , Name               -- Names (e.g., for binders/identifiers).
  , Term               -- Terms are contexts without holes.
  , lCtxLabel          -- Project label from labelled AST.
  , lCtxLabelCombine   -- Combine labels for labelled ASTs.
  , lCtxLabelCombine'  -- Combine multiple labels for labelled ASTs.
  ) where

import CtxKind (CtxKind)

{-
  <TO-DO>: - How can GADTs help?
        
  Information:
  -----------------------------------------------------------------------------
  - Abstract syntax of the source language manipulated by UNIE;
  - For now we use variable binders '\x' for lambda abstractions (cf. De 
    Bruijn notation) because ultimately we want maximum readability of 
    improvement proofs. This may change in the future, e.g., using De Bruijn 
    internally and convert to variables in show instances;
  - UNIE accepts a more liberal source language than that defined in the paper. 
    For example, function arguments need need not be variables, i.e., we don't 
    have App Ctx <var> and instead we have App Ctx Ctx. However this is just 
    syntactic sugar and in certain circumstances (e.g., during reasoning steps) 
    syntactic sugar must be /normalised/ (such that the resulting language 
    matches that defined in the paper precisely) before transformations can be 
    applied (see Normalisation). This approach is reflected throughout UNIE's 
    implementation in order to increase its usability;
  - For a breakdown of naming conventions, see SourceParserLib.
-}

-------------------------------------------------------------------------------
-- Datatypes: --
-------------------------------------------------------------------------------
 
type Name = String  -- Binders/identifiers are strings.
type Idx  = Int     -- Indices for let bindings/case alternatives (used
                    -- for navigation purposes).

{- 
  A word on global bindings:
  -----------------------------------------------------------------------------
  - Global bindings :: GBind give memorable names to contexts and are
    defined by users in source files and then imported into the system. 
    However, note that a recursive function /cannot/ be defined via applied 
    occurrences of its identifier i.e., f = ..f.. in a typical Haskell fashion. 
    Instead any recursion /must/ be defined with let bindings i.e., 
    let x = ..x.. in .. (as per our operational model, see paper);
  - TBinds abstract out let bindings. E.g., given a term:
     
     let 
        x_0 = M_0
        x_1 = M_1 
        x_2 = M_2
     in N

    this can be be replaced by N, provided x_i = M_i are defined as TBinds
    and imported into the system. Therefore TBinds provide a sort of 
    library of /term/ definitions. (Scoping is handled in the obvious 
    manner.) Thus, while reasoning in improvement proofs, TBinds added to 
    the heap in the same way let bindings are. I.e., for a list of TBinds 
    x_ = M_ and a term N  being transformed, we assume let x_ = M_ in N;
  - CBinds are just shorthand notation, E.g., given a CBind:

      C_C = \x.[-] (where C is the standard context kind),

    C_C[x] is shorthand for \x.x, so they can be substituted for each other
    /freely/ during improvement proofs. Thus, similarly, CBinds provide a 
    library of /context/ definitions. An applied occurrence of a CBind's
    identifier i.e., C_C above requires /additional/ information regarding what 
    (if anything) has been substituted into the context's holes, i.e., [x] 
    in our example. Thus we have a special constructor for this purpose called 
    a CVar (context variable), written C_C[x] in the source language. 
-}
data GBind = CBind CtxKind Name Ctx  -- <kind>_<name> = <ctx> (see CtxKind)
           | TBind Name Term         --        <name> = <term>
             deriving Eq
 
-- Terms and contexts are the main datatypes used when manipulating the 
-- source language.
type Term = Ctx  -- A term is simply a context that doesn't contain any holes.

-- Note we have two built-in data types: integer/string literals
data Ctx   
 = Var    Name                       -- Variables                
 | LitInt Int                        -- Integer literals
 | LitStr String                     -- String literals
 | Abs    Name Ctx                   -- Abstractions
 | App    Ctx Ctx                    -- Applications  
 | Tick   Ctx                        -- Tick annotations
 | Let    [Bind] Ctx                 -- Let statements
 | Case   Ctx [Alt]                  -- Case statements
 | AppD   Con [Term]                 -- Data types (the application of a data 
                                     -- constructor to a list of /term/ arguments)
 | Hole                              -- Holes
 | CVar   CtxKind Name (Maybe Ctx)   -- 'Context variables'. These work in the same way as 
   deriving Eq                       -- variables, in that they refer to CBinds just as
                                     -- variables can refer to TBinds. The Maybe Ctx captures 
                                     -- whether the context's holes have been substituted.
                                                             
-- Binds/Alts store their indices for navigation purposes.
data Bind = Bind Name Ctx Idx    -- A let binding binds a context to a variable, i.e., <name> = <ctx> 
            deriving Eq          -- Note that lists of let bindings are /mutually/ recursive.

instance Ord Bind where 
  (Bind _ _ idx1) `compare` (Bind _ _ idx2) = idx1 `compare` idx2

{- 
  Alternatives for pattern matching in case statements. Important note: here we 
  force data constructors to be applied to /variables/ (unlike in for AppDs). 
  This means you can't pattern match on list sugared syntax. 

  E.g., case .. of { [1,2] -> .. ; .. } is invalid. 
-}
data Alt = Alt Con [Name] Ctx Idx  
           deriving Eq

instance Ord Alt where 
  (Alt _ _ _ idx1) `compare` (Alt _ _ _ idx2) = idx1 `compare` idx2

-- Constructors used for data types/pattern matching.
data Con   
  = VARIABLE        -- Pattern match and bind the result to a variable.
  | CONS                                
  | NIL                                
  | DEFAULT         -- Pattern matching default i.e., '_ -> ..'
  | LITINT Int                        
  | LITSTR String                       
    deriving Eq

-------------------------------------------------------------------------------
-- Labelled abstract syntax: --
-------------------------------------------------------------------------------
{-
  - We extend the datatypes above to store labels at each node of the AST;
  - These labels can be anything, but so far our main use is for storing
    "show settings", which give us complete control over how each component of 
    the AST is pretty printed, allowing us to colourise/stylise as we please.
-}

-- One-to-one with GBind/Ctx/Bind/Alt: -- 

data L_GBind a = L_CBind (CtxKind, Name, L_Ctx a, a)
               | L_TBind (Name, L_Ctx a, a) 
                 deriving Eq

data L_Ctx a   
  = L_Var (Name, a)                               
  | L_LitInt (Int, a)                  
  | L_LitStr (String, a)                  
  | L_Abs    (Name, L_Ctx a, a)            
  | L_App    (L_Ctx a, L_Ctx a, a)               
  | L_Tick   (L_Ctx a, a)                   
  | L_Let    ([L_Bind a], L_Ctx a, a)           
  | L_Case   (L_Ctx a, [L_Alt a], a)                
  | L_AppD   (Con, [L_Ctx a], a)                                       
  | L_Hole   a                     
  | L_CVar   (CtxKind, Name, Maybe (L_Ctx a), a) 
    deriving Eq                    
                                               
data L_Bind a = L_Bind (Name, L_Ctx a, Idx, a)   
                deriving Eq         

data L_Alt a  = L_Alt (Con, [Name], L_Ctx a, Idx, a) 
                deriving Eq

-- Helpers: -------------------------------------------------------------------

-- Project out an L_Ctx's label.
lCtxLabel :: L_Ctx a -> a 
lCtxLabel (L_Var (_, x))        = x
lCtxLabel (L_LitInt (_, x))     = x
lCtxLabel (L_LitStr (_, x))     = x 
lCtxLabel (L_Abs (_, _, x))     = x 
lCtxLabel (L_App (_, _, x))     = x 
lCtxLabel (L_Tick (_, x))       = x 
lCtxLabel (L_Let (_, _, x))     = x 
lCtxLabel (L_Case (_, _, x))    = x 
lCtxLabel (L_AppD (_, _, x))    = x 
lCtxLabel (L_Hole x)            = x 
lCtxLabel (L_CVar (_, _, _, x)) = x 

-- Given a combine function f, combine an L_Ctx with another label of
-- the same type. Note: ideally f should be commutative.
lCtxLabelCombine :: (a -> a -> a) -> a -> L_Ctx a -> L_Ctx a
lCtxLabelCombine f x' (L_Var (ns, x))            = L_Var (ns, f x x')
lCtxLabelCombine f x' (L_LitInt (i, x))          = L_LitInt (i, f x x')
lCtxLabelCombine f x' (L_LitStr (s, x))          = L_LitStr (s, f x x')
lCtxLabelCombine f x' (L_Abs (ns, lctx, x))      = L_Abs (ns, lctx, f x x')
lCtxLabelCombine f x' (L_App (lc1, lc2, x))      = L_App (lc1, lc2, f x x')
lCtxLabelCombine f x' (L_Tick (lctx, x))         = L_Tick (lctx, f x x')
lCtxLabelCombine f x' (L_Let (lbs, lctx, x))     = L_Let (lbs, lctx, f x x')
lCtxLabelCombine f x' (L_Case (lctx, las, x))    = L_Case (lctx, las, f x x')
lCtxLabelCombine f x' (L_AppD (con, lcs, x))     = L_AppD (con, lcs, f x x')
lCtxLabelCombine f x' (L_Hole x)                 = L_Hole (f x x') 
lCtxLabelCombine f x' (L_CVar (k, ns, mlctx, x)) = L_CVar (k, ns, mlctx, f x x')

-- Generalised version of above.
lCtxLabelCombine' :: (a -> a -> a) -> [a] -> L_Ctx a -> L_Ctx a
lCtxLabelCombine' f xs lctx = foldr (lCtxLabelCombine f) lctx xs