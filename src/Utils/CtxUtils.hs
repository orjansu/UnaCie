{-# LANGUAGE TupleSections #-}

module CtxUtils 
  ( altCtx        -- Project out context from alt.
  , altNames      -- Project out all bound vars. in an alt.
  , altsCtxs      -- Project out contexts from alts.
  , bindBinder    -- Project out a binding's binder.
  , bindCtx       -- Project out context from alt.
  , bindsCtxs     -- Project out contexts from bindings.
  , bindsToPairs  -- Convert a list of bindings to a list of (Name, Ctx) pairs.
  , ctxBinders    -- Project out all bound vars. from a context.

  -- Checking if datatypes have holes: --

  , hasHoles
  , hasHolesAlt
  , hasHolesAlts
  , hasHolesBind
  , hasHolesBinds

  -- Checking syntactic form: --

  , isAbs
  , isApp
  , isAppD
  , isAtomic
  , isCVar
  , isCase
  , isDatatype
  , isFunApp
  , isHole
  , isLet
  , isList
  , isLitInt
  , isNonSubstCVar  -- Is context variable with empty holes.
  , isNormDatatype
  , isRedex
  , isSpecCVar     -- Is 'specific' context variable.
  , isSpecVar       -- Is 'specific' variable.
  , isTerm
  , isTick
  , isVal
  , isVar
  , isVarList

  , lookupBind           -- Lookup a particular binding in a let statement.
  , nestingFilter        -- Filtering incompatible context nestings.
  , nil                  -- Empty list.
  , pairsToBinds         -- Converting a list of (Name, Ctx) pairs to binds.
  , reindexBinds         -- Reindexing a list of bindings.
  , substAlt             -- Subst. holes in an alt.
  , substBind            -- Subst. holes in a bind.
  , substCtx             -- Subst. holes in a ctx.
  , substCtx'            -- Subst. holes in a ctx, Maybe sub param.
  , validateCtxNestings  -- Ensuring context nestings are compatible.
  ) where 

import CtxAST  ( Alt(..), Bind(..), Con(..)
               , Ctx(..), Name, Term )
import CtxKind (CtxKind(..))
import Utils   (allEq, safeHead, notNull)

import Data.List (union)

{-
  <TO-DO>: - Checking syntactic forms should be generalised using constructor
             patterns.

  Information:
  -----------------------------------------------------------------------------
  - Helper functions for manipulating contexts.

  Working notes:
  -----------------------------------------------------------------------------
  - Some of the functions here are "duplicated" as transformations/rewrites, 
    but have a different implementation (i.e., are not liftings of the 
    functions in this file). This is because their definitions (in TransUtils) 
    make use of congruence combinators from the KURE module, which handle KURE 
    context management and generating appropriate error messages on failure. 
    This would not be possible if we just lifted these implementations using
    liftT/liftR.
-}

-------------------------------------------------------------------------------
-- Context generation/context matching helpers: -- 
-------------------------------------------------------------------------------

-- Ensuring nestings/substitutions are compatible: ---------------------------- 

{- 
  - Ensure a list of context nestings are /compatible/;
    - A compatible list of nestings is such that the nestings are all the same 
      once empty nestings (i.e., []) have been removed, e.g.,

       [[x,y], [], [], [x,y]] is a compatible list of nestings, 
       [[x,y], [], [], [x]]   is not compatible as [x,y] /= [x];

    - However, we must also take into consideration the occurrence of empty 
      holes, (the Bool parameter), as a substitutions /cannot/ be mixed with
      empty holes. This means that when the Bool is True, all nestings must
      be empty for validateCtxNestings to succeed.
  - If the nestings are compatible, we select a single nesting to be 
    representative of the list;
  - This is used when validating contexts with /multiple/ holes, as 
    substitutions into each of the holes must be equal. 
-}
validateCtxNestings :: Bool -> [[Ctx]] -> Maybe [Ctx]
validateCtxNestings True ctxss 
 | null ctxss' = Just [] 
 | otherwise   = Nothing
 where ctxss' = filter notNull ctxss 
validateCtxNestings False ctxss  
 | null ctxss'  = Just []
 | allEq ctxss' = Just (head ctxss')
 | otherwise    = Nothing
 where ctxss' = filter notNull ctxss 

-- In practice we often we work with 3-tuples and filter invalid nestings.
nestingFilter :: (a, [[Ctx]], Bool) -> Maybe (a, [Ctx], Bool)
nestingFilter (x, ctxss, b) = (x, , b) <$> validateCtxNestings b ctxss 
                              
-------------------------------------------------------------------------------
-- CtxAST helpers: -- 
-------------------------------------------------------------------------------

-- Check the syntactic form of contexts: --------------------------------------

-- Values of the language are abstractions, 
-- datatypes and value CVars.
isVal :: Ctx -> Bool 
isVal Abs{}                   = True
isVal (CVar VAL _ (Just sub)) = isTerm sub
isVal ctx                     = isDatatype ctx

-- Sugared datatypes are valid as long as 
-- their elements are /atomic/.
isAtomic :: Ctx -> Bool 
isAtomic LitInt{} = True
isAtomic LitStr{} = True 
isAtomic Var{}    = True
isAtomic _        = False

isVar :: Ctx -> Bool 
isVar Var{} = True 
isVar _     = False

isAbs :: Ctx -> Bool 
isAbs Abs{} = True 
isAbs _     = False 

isApp :: Ctx -> Bool 
isApp App{} = True 
isApp _     = False

isTick :: Ctx -> Bool 
isTick Tick{} = True 
isTick _      = False

isLet :: Ctx -> Bool 
isLet Let{} = True 
isLet _     = False

isCase :: Ctx -> Bool 
isCase Case{} = True 
isCase _      = False

isHole :: Ctx -> Bool 
isHole Hole = True 
isHole _    = False

isCVar :: Ctx -> Bool 
isCVar CVar{} = True 
isCVar _      = False

isAppD :: Ctx -> Bool 
isAppD AppD{} = True 
isAppD _      = False  

isLitInt :: Ctx -> Bool
isLitInt LitInt{} = True 
isLitInt _        = False

-- The Ctx datatype doesn't prevent us from putting any ctx/term
-- inside an AppD's [Term]. Parsers we use prevent this, but sometimes 
-- we utilise additional checks.
isDatatype :: Ctx -> Bool 
isDatatype LitInt{}        = True 
isDatatype LitStr{}        = True
isDatatype l@(AppD NIL _)  = isList l 
isDatatype l@(AppD CONS _) = isList l 
isDatatype _               = False

{-
  Strictly speaking, the source language only permits data 
  type constructors to be applied to (lists of) variables, 
  but we introduce some syntactic sugar for lists. This 
  side-lines definitions such as:

    let a = [] 
        b = 1
        c = b :: a
        d = 2
        e = d :: c
      ..
        z = .. 
     in z

  in favour of
    
    [1,2,3,..,26].

  As such, we allow lists to be lists of 'atoms', where an 
  atom is a LitInt, LitStr or Var. The CONS constructor
  can be thus applied to atoms and previously defined lists.
-}
isList :: Ctx -> Bool 
isList (AppD NIL [])  = True 
isList (AppD CONS ts) | length ts == 2 = all (\t -> isAtomic t || isList t) ts
isList _              = False

-- As above but checks for variables only.
isVarList :: Ctx -> Bool 
isVarList (AppD NIL [])  = True 
isVarList (AppD CONS ts) | length ts == 2 = all (\t -> isVar t || isVarList t) ts
isVarList _              = False

-- Normalised datatype.
isNormDatatype :: Ctx -> Bool 
isNormDatatype LitInt{}        = True 
isNormDatatype LitStr{}        = True
isNormDatatype l@(AppD NIL _)  = isVarList l 
isNormDatatype l@(AppD CONS _) = isVarList l 
isNormDatatype _               = False

-- Check if a Ctx is a Term i.e., has no holes
-- Also check AppD is valid.
isTerm :: Ctx -> Bool 
isTerm  = go 
 where
  go Var{}                  = True
  go LitInt{}               = True
  go LitStr{}               = True
  go d@AppD{}               = isDatatype d
  go Hole                   = False
  go (Abs _ ctx)            = go ctx 
  go (App c1 c2)            = go c1 && go c2
  go (Tick ctx)             = go ctx 
  go (Let bs ctx)           = all (\(Bind _ ctx _) -> go ctx) bs && go ctx 
  go (Case ctx as)          = go ctx && all (\(Alt _ _ ctx _) -> go ctx) as
  go (CVar _ _ Nothing)     = False
  go (CVar _ _ (Just body)) = go body

-- Check if the given context is a reducible expression (redex).
isRedex :: Ctx -> Bool 
isRedex (App Abs{} _) = True 
isRedex _             = False

-- Check if the given context is a function application.
isFunApp :: Ctx -> Bool 
isFunApp (App Abs{} _)    = True 
isFunApp (App c1@App{} _) = isFunApp c1
isFunApp _                = False  

-- Specific variables: --------------------------------------------------------

isSpecVar :: Name -> Ctx -> Bool 
isSpecVar ns (Var ns') = ns == ns'
isSpecVar _ _          = False

isSpecCVar :: CtxKind -> Name -> Ctx -> Bool 
isSpecCVar k ns (CVar k' ns' _) = k == k' && ns == ns'
isSpecCVar _ _ _                = False

-- Binds: ---------------------------------------------------------------------

-- Project name from bind.
bindBinder :: Bind -> Name 
bindBinder (Bind ns _ _) = ns

-- Project context from bind.
bindCtx :: Bind -> Ctx 
bindCtx (Bind _ ctx _) = ctx 

-- Project contexts from binds.
bindsCtxs :: [Bind] -> [Ctx]
bindsCtxs  = fmap bindCtx

-- Convert a list of binds to a list of pairs.
bindsToPairs :: [Bind] -> [(Name, Ctx)]
bindsToPairs  = fmap (\(Bind ns ctx _) -> (ns, ctx))

-- Convert a list of pairs to a list of binds.
pairsToBinds :: [(Name, Ctx)] -> [Bind]
pairsToBinds  = fmap (\(idx, (ns, ctx)) -> Bind ns ctx idx) . zip [0..]

-- Add consecutive indices to bindings.
reindexBinds :: [Bind] -> [Bind]
reindexBinds  = (f <$>) . zip [0..]
                where f (idx, Bind ns ctx _) = Bind ns ctx idx  

-- Search for a specific binding .
lookupBind :: Name -> Ctx -> Maybe Bind 
lookupBind ns (Let bs _) = safeHead $ filter (\b -> bindBinder b == ns) bs
lookupBind _ _           = Nothing 

-- Alts: ----------------------------------------------------------------------

-- Project names from alternative.
altNames :: Alt -> [Name]
altNames (Alt _ nss _ _) = nss

-- Project context from alt.
altCtx :: Alt -> Ctx 
altCtx (Alt _ _ ctx _) = ctx 

-- Project contexts from alts.
altsCtxs :: [Alt] -> [Ctx]
altsCtxs  = fmap altCtx
 
-- Binders: -------------------------------------------------------------------

-- Project out all binders from a context
ctxBinders :: Ctx -> [Name]
ctxBinders  = go 
 where 
  go Var{}           = []
  go LitInt{}        = [] 
  go LitStr{}        = []
  go Hole            = []
  go (AppD _ terms)  = concatMap go terms
  go (Abs ns ctx)    = [ns]  `union` go ctx 
  go (App c1 c2)     = go c1 `union` go c2 
  go (Tick ctx)      = go ctx 
  go (Let bs ctx)    = concatMap (\(Bind ns ctx _) -> [ns] 
                        `union` go ctx) bs 
                        `union` go ctx 
  go (Case ctx as)   = concatMap (\(Alt _ nss ctx _) -> nss 
                        `union` go ctx) as 
                        `union` go ctx 
  go (CVar _ _ body) = maybe [] go body 

-------------------------------------------------------------------------------
-- Holes: -- 
-------------------------------------------------------------------------------

-- Check if certain datatypes/lists of datatypes have holes: --

hasHoles :: Ctx -> Bool 
hasHoles  = (> 0) . numHoles

hasHolesBind :: Bind -> Bool 
hasHolesBind (Bind _ ctx _) = hasHoles ctx

hasHolesBinds :: [Bind] -> Bool
hasHolesBinds  = any hasHolesBind

hasHolesAlt  :: Alt -> Bool 
hasHolesAlt (Alt _ _ ctx _) = hasHoles ctx

hasHolesAlts :: [Alt] -> Bool 
hasHolesAlts  = any hasHolesAlt

-- Calculate the number of holes in a Ctx.
numHoles :: Ctx -> Int 
numHoles ctx = go [ctx] 0 
 where 
  go []                         n =                                   n
  go (Var{}               : cs) n = go cs                             n 
  go (LitInt{}            : cs) n = go cs                             n
  go (LitStr{}            : cs) n = go cs                             n
  go (AppD _ terms        : cs) n = go (terms  ++ cs)                 n                      
  go (Abs _ ctx           : cs) n = go (ctx     : cs)                 n 
  go (App c1 c2           : cs) n = go (c1 : c2 : cs)                 n
  go (Tick ctx            : cs) n = go (ctx     : cs)                 n
  go (Let bs ctx          : cs) n = go (ctx     : bindsCtxs bs ++ cs) n 
  go (Case ctx as         : cs) n = go (ctx     : altsCtxs  as ++ cs) n 
  go (CVar _ _ (Just ctx) : cs) n = go (ctx     : cs)                 n 

  go (Hole                : cs) n = go cs                             (n + 1)
  go (CVar _ _ Nothing    : cs) n = go cs                             (n + 1)     

-- Check whether a CVar's holes have been substituted.
isNonSubstCVar :: Ctx -> Bool 
isNonSubstCVar (CVar _ _ Nothing) = True 
isNonSubstCVar _                  = False

{-  
  - Substitute /all/ holes in a given context;
  - To substitute a hole, we simply replace the hole constructor by the given 
    substitution. Specifically, we don't account for variable capture (cf. safe 
    substitution for variables), as variable capture is the expected result 
    should it occur;
  - Filling a hole in a CVar is done /implicitly/, thus we change its syntax 
    from C[-] to C[sub].
-}
substCtx :: Ctx -> Ctx -> Ctx 
substCtx sub = go
 where 
  -- Substitution cases:
  go Hole                    = sub 
  go (CVar k ns Nothing)     = CVar k ns (Just sub)
  -- Recursive cases:
  go (Abs ns ctx)            = Abs ns (go ctx)
  go (App c1 c2)             = App (go c1) (go c2) 
  go (Tick ctx)              = Tick (go ctx) 
  go (Let bs ctx)            = Let (fmap (substBind sub) bs) (go ctx)
  go (Case ctx as)           = Case (go ctx) (fmap (substAlt sub) as)
  go (AppD con cs)           = AppD con (fmap go cs)
  go (CVar k ns (Just body)) = CVar k ns (Just $ go body)
  -- Other cases:
  go ctx                     = ctx

-- As above, but takes substitution in Maybe form, as from CVars.
substCtx' :: Maybe Ctx -> Ctx -> Ctx 
substCtx' Nothing    = id 
substCtx' (Just sub) = substCtx sub

-- Substitute holes inside a binding.
substBind :: Ctx -> Bind -> Bind
substBind sub (Bind ns body idx) = Bind ns (substCtx sub body) idx

-- Substitute holes inside an alt.
substAlt :: Ctx -> Alt -> Alt
substAlt sub (Alt con nss ctx idx) = Alt con nss (substCtx sub ctx) idx

-- Misc.: ---------------------------------------------------------------------

-- The empty list.
nil :: Term 
nil  = AppD NIL []