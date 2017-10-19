
module CtxPatAST 
  ( AltPat(..)       -- Case alternative patterns.
  , BindPat(..)      -- Let binding patterns.
  , CtxConstPat(..)  -- Constructor patterns.
  , CtxPat(..)       -- Context patterns.
  , UPat(..)         -- Universe U patterns.
  , altPatsCtxs      -- Project context patterns from alt. patterns.
  , altToAltPat      -- Convert an alt. to an alt pattern.
  , bindPatBinder    -- A let binding pattern's binder.
  , bindPatsCtxs     -- Project context patterns from bind patterns.
  , bindToBindPat    -- Convert a bind to a bind pattern.
  , ctxToCtxPat      -- Convert a context to a context pattern.
  , hasHoles         -- Check if a pattern has holes.
  , numHoles         -- Count number of holes in a pattern.
  ) where

import CtxAST (Alt(..), Bind(..), Con, Ctx(..), Name)

{-
   <TO-DO>: - Are KindPats useful?
            -- If so, how do we handle their holes/substitutions?
            -- It appears to me that they would be useful for cost-equiv. 
               contexts.

   Information:
   ------------
   - Abstract syntax for context patterns;
   - Context patterns are used to specify contexts by shorthand notation
     (see paper);
   - They are typically input by the user on the command line as a parameter
     to a transformation rule. 
-}

-------------------------------------------------------------------------------
-- Data types: --
-------------------------------------------------------------------------------

-- Universe type U patterns (without GBind as not needed).
data UPat = UCtxPat  CtxPat 
          | UBindPat BindPat 
          | UAltPat  AltPat
            deriving Eq

-- Context patterns are one-to-one with Ctxs, but with additions.
data CtxPat  
  =  PVar     Name               -- Variables
  |  PLitInt  Int                -- Integer literals
  |  PLitStr  String             -- String literals
  |  PAbs     Name CtxPat        -- Abstractions
  |  PApp     CtxPat CtxPat      -- Applications
  |  PTick    CtxPat             -- Tick annotations
  |  PLet     [BindPat] CtxPat   -- Let statements
  |  PCase    CtxPat [AltPat]    -- Case statements
  |  PAppD    Con [CtxPat]       -- Data types

  -- Holes in patterns can be substituted to specify context 'nestings'.    
  |  PHole    (Maybe CtxPat)                       

  -- Context variables, Bool indicates whether subst. in PCVar is 'nested'.
  |  PCVar    Name (Maybe CtxPat) Bool             

  -- Constructor patterns, match contexts based on their constructors only.
  |  ConstPat CtxConstPat  

  -- Wildcards, match /any/ context.
  |  Wildcard                                     
     deriving Eq                  
  
-- Note: bind/alt patterns do not have indices because we don't need
-- to navigate over them (cf. contexts).

-- Let binding patterns.                                                                           
data BindPat = PBind Name CtxPat       
             | BindWildcard  -- Matches /1/ or more let bindings.
               deriving Eq 

-- Case alternative patterns.
data AltPat = PAlt Con [Name] CtxPat   
            | AltWildcard    -- Matches /1/ or more alternatives.        
              deriving Eq 

-- Constructor patterns match a context based on its constructor only;
-- Hole/CVar omitted for now as I can't see why they'd be useful.
data CtxConstPat  
  = P_VAR  
  | P_LIT_INT
  | P_LIT_STR
  | P_ABS 
  | P_APP
  | P_TICK
  | P_LET
  | P_CASE
  | P_DATA
  | P_LIST
    deriving Eq

{-

'Context kind' patterns match any context of the specified kind.
This is ommited for the time being, as I don't have a use case for it.

data CtxPat = .. | KindPat CtxKindPat

data CtxKindPat  
  = P_K_STD 
  | P_K_VAL 
  | P_K_EVAL
  | P_K_APP
    deriving Eq
-}

-------------------------------------------------------------------------------
-- Helpers: --
-------------------------------------------------------------------------------

-- Project out a let binding's binder.
bindPatBinder :: BindPat -> Maybe Name 
bindPatBinder (PBind ns _) = Just ns
bindPatBinder BindWildcard = Nothing

-- Project the context patterns from Bind/Alt patterns: --

bindPatsCtxs :: [BindPat] -> [CtxPat]
bindPatsCtxs = concatMap f 
               where 
                 f BindWildcard  = []
                 f (PBind _ ctx) = [ctx]

altPatsCtxs :: [AltPat] -> [CtxPat]
altPatsCtxs = concatMap f
              where 
                f AltWildcard    = []
                f (PAlt _ _ ctx) = [ctx] 
                          
-- Calculating holes: --

-- Check if a CtxPat has one or more holes.
hasHoles :: CtxPat -> Bool                                     
hasHoles  = (> 0) . numHoles

-- Calculate the number of holes in a CtxPat.
numHoles :: CtxPat -> Int 
numHoles ctx = go [ctx] 0 
  where 
    go :: [CtxPat] -> Int -> Int
    go []                   n =                   n
    go (PVar{}        : cs) n = go cs             n 
    go (PLitInt{}     : cs) n = go cs             n
    go (PLitStr{}     : cs) n = go cs             n
    go (PAppD{}       : cs) n = go cs             n  
    go (ConstPat{}    : cs) n = go cs             n       
    go (Wildcard      : cs) n = go cs             n                      
    go (PAbs _ ctx    : cs) n = go (ctx : cs)     n 
    go (PApp c1 c2    : cs) n = go (c1 : c2 : cs) n
    go (PTick ctx     : cs) n = go (ctx : cs)     n

    go (PLet pbs ctx  : cs) n = go (ctx : bindPatsCtxs pbs ++ cs) n 
    go (PCase ctx pas : cs) n = go (ctx : altPatsCtxs pas ++ cs)  n  
    go (PCVar _ (Just ctx) False : cs) n = go (ctx : cs)          n

    -- Holes as per non-pattern contexts.
    go (PHole Nothing     : cs) n = go cs (n + 1)
    go (PCVar _ Nothing _ : cs) n = go cs (n + 1) 

    -- Substitutions /lead/ to holes when context is generated.
    go (PHole (Just ctx)        : cs) n = go (ctx : cs) (n + 1)  
    go (PCVar _ (Just ctx) True : cs) n = go (ctx : cs) (n + 1)  

-- Converting non-patterns to patterns: --

ctxToCtxPat                 :: Ctx -> CtxPat 
ctxToCtxPat (Var ns)         = PVar ns
ctxToCtxPat (LitInt i)       = PLitInt i 
ctxToCtxPat (LitStr s)       = PLitStr s
ctxToCtxPat (Abs ns ctx)     = PAbs ns (ctxToCtxPat ctx)
ctxToCtxPat (App c1 c2)      = PApp (ctxToCtxPat c1) (ctxToCtxPat c2)
ctxToCtxPat (Tick ctx)       = PTick (ctxToCtxPat ctx)
ctxToCtxPat (Let bs ctx)     = PLet (bindsToBindPats bs) (ctxToCtxPat ctx)
ctxToCtxPat (Case ctx as)    = PCase (ctxToCtxPat ctx) (altsToAltPats as)
ctxToCtxPat (AppD con ctxs)  = PAppD con (fmap ctxToCtxPat ctxs)
ctxToCtxPat Hole             = PHole Nothing
ctxToCtxPat (CVar _ ns mctx) = PCVar ns (fmap ctxToCtxPat mctx) False 

bindsToBindPats :: [Bind] -> [BindPat] 
bindsToBindPats  = fmap bindToBindPat

bindToBindPat :: Bind -> BindPat
bindToBindPat (Bind ns ctx _) = PBind ns (ctxToCtxPat ctx)

altsToAltPats :: [Alt] -> [AltPat]
altsToAltPats  = fmap altToAltPat 

altToAltPat :: Alt -> AltPat 
altToAltPat (Alt con ns ctx _) = PAlt con ns (ctxToCtxPat ctx)