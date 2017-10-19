{-# LANGUAGE FlexibleContexts #-}

module CtxPatMatch 
 ( -- * Generators
   altPatsGen        -- Generate all valid contexts from an alt. pattern.
 , altPatsMatchGen   -- Generate a context from an alt pattern (if possible).
 , bindPatsGen       -- Generate all valid contexts from an bind pattern.
 , bindPatsMatchGen  -- Generate a context from a bind pattern (if possible).
 , ctxPatGen         -- Generate all valid contexts from a context pattern.
 , ctxPatMatchGen    -- Generate a context from a context pattern (if possible).
 -- * 
 , ctxPatPathsT      -- Find all paths to a specific context pattern.
 , eqAltPat          -- Match against an alt. pattern.
 , eqAltPats         -- As above but for a list of alt. patterns.
 , eqBindPat         -- Match against a bind pattern.
 , eqBindPats        -- As above but for a list of bind patterns.
 , eqConstPat        -- Match against a constructor pattern.
 , eqCtxPat          -- Match against a context pattern.
 , patSeqGen         -- Helper for generate with sequences of patterns.
 ) where 

import Classes   (AddBinders)
import Crumb     (Crumb)
import CtxAST    (Alt(..), Bind(..), Ctx(..))
import CtxPatAST (AltPat(..), BindPat(..), CtxConstPat(..), CtxPat(..))
import CtxUtils  ( hasHoles, hasHolesAlt, hasHolesAlts
                 , hasHolesBind, hasHolesBinds, isList
                 , nestingFilter, validateCtxNestings )
import Kure      ()
import KureExtra (liftT)
import Universes (U(..))

import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Language.KURE.Pathfinder (pathsToT)
import Language.KURE.ExtendableContext (ExtendContext)
import Language.KURE ( ExtendPath, LocalPath, MonadCatch
                     , ReadPath, Transform, promoteT )

{-
  <TO-DO>: - Some paths functions omitted for now, do we need them?
           - Notable lack of code comments throughout this file (apologies). 

  Information:
  -----------------------------------------------------------------------------
  - Context matching mechanisms, see paper for overview of context patterns.

  Working notes:
  -----------------------------------------------------------------------------
  - The context pattern matching algorithm doesn't care whether its results 
    correspond to contexts (nestings) of particular /kinds/, it just naively 
    matches on syntax;
  - It does however only generate nestings which are 'syntactically 
    congruent', i.e., if a term has multiple holes that are /substituted/, 
    the substitutions must be equal, as this is a fundamental rule of the
    language. If matching over contexts with empty holes, it also prevents 
    mixing empty holes with substitutions, as this would also be fundamentally 
    erroneous w.r.t. syntactic congruity;
  - The CtxCheck module is typically used to verify the results of the matching 
    algorithm. In the future, these processes can probably be fused together, 
    but efficiency isn't an issue for the time being.
-}

-------------------------------------------------------------------------------
-- Context pattern /checking/: --
-------------------------------------------------------------------------------
{- 
  - We equate a context pattern with a context by matching syntactic 
    sub-structure;
  - We take wildcards and empty holes as place-holders for /any/ sub-structure. 
    Anything else must match precisely, with two exceptions:
    (1) We don't account for bind/alt indices as we assume their relative 
        positions are preserved by the pattern. In fact, context patterns have 
        no appreciation for bind/alt indices whatsoever;
    (2) We don't account for context kinds, as we assume the pattern refers
        to the context kinds present in the given term. In fact, context 
        patterns have no appreciation for context kinds whatsoever.
  - Note that we have to be sure that /nested/ substitutions are equal and 
    cannot mix empty hole occurrences with substitutions (in cases where
    the context we are matching over has empty holes).
-}

{- Contexts --
  - As an example, let us consider contexts:
    - We are only interested if the pattern pctx matches the context ctx,
      however, we require more information to decide whether this is the case;
    - We thus use a helper function eqCtxPat' that returns (Bool, [Ctx], Bool) 
      and project out the first element of the 3-tuple at the top level:
      - The first element of the 3-tuple is whether the sub-structure in
      question matches against the pattern's sub-structure (i.e., in 
      isolation). A False appearing anywhere in this field would ideally 
      short-cut the rest of the computation as it will always evaluate to
      False;
      - The second element is the substitution nesting associated with this
        particular matching, e.g., 

        [-] matches against x and returns (True, [x], False), here [x] is
        the substitution nesting;

      - The third element signifies the occurrence of an empty hole, e.g., 

        [-] matches against [-] and returns (True, [], True).

    - For constructors with multiple children e.g., applications, we must
      ensure that each child node matches against the pattern and then validate 
      the substitutions of each child to ensure any substituted holes agree on 
      their substitutions, and, furthermore, ensure we aren't mixing empty hole 
      occurrences with substitutions:
      - In the case of applications, we match on both child nodes and then call 
        nestingFilter to do the appropriate substitution validation. E.g., 
        
        [-] [-] does not match [-] x 

        LHS: [-] against [-] would return (True, [], True) 
        RHS: [-] against x   would return (True, [x], False).
        
        Here we are mixing substitutions with empty hole occurrences:
        nestingFilter True [[], [x]] returns Nothing.

        On the other hand, these both match:

        [-] [-] against x x 
        [-] [-] against [-] [-].

  - Note that as the context pattern could relate to a context nesting, each 
    time we go one level deeper in the nesting, we reset the empty hole 
    occurrence to /False/. This is because we cannot mix empty hole occurrences
    and substitutions at each nesting level, but between levels there are no 
    such restrictions. It is trivially the case that for valid matches, empty 
    holes can only occur at the deepest level of the nesting. E.g., 

      [|x [-]|] matches against x [-] and returns (True, [x [-]], False).
-}

-- Contexts: ------------------------------------------------------------------

eqCtxPat :: CtxPat -> Ctx -> Bool 
eqCtxPat pctx ctx = let (b, _, _) = eqCtxPat' pctx ctx in b

eqCtxPat' :: CtxPat -> Ctx -> (Bool, [Ctx], Bool)

-- Wildcards match anything.
eqCtxPat' Wildcard ctx = (True, [], CtxUtils.hasHoles ctx)

-- Empty holes match anything w.r.t. syntactic congruity.            
eqCtxPat' (PHole Nothing) Hole = (True, [], True)

-- We reset the empty hole occurrence bool when we go one nesting deeper.
eqCtxPat' (PHole Nothing) ctx = (True, [ctx], False) -- Set to False

eqCtxPat' (PHole (Just pctx)) ctx = (b, ctx : cs, False) -- Set to False
                                    where (b, cs, _) = eqCtxPat' pctx ctx

eqCtxPat' (PVar ns1)   (Var ns2)   = (ns1 == ns2, [], False)
eqCtxPat' (PLitInt i1) (LitInt i2) = (i1  == i2,  [], False)
eqCtxPat' (PLitStr s1) (LitStr s2) = (s1  == s2,  [], False)

eqCtxPat' (PAbs ns1 pctx) (Abs ns2 ctx) | ns1 == ns2 = eqCtxPat' pctx ctx

eqCtxPat' (PTick pctx) (Tick ctx) = eqCtxPat' pctx ctx 

eqCtxPat' (ConstPat pct) ctx = (eqConstPat pct ctx, [], CtxUtils.hasHoles ctx)

eqCtxPat' (PApp p1 p2) (App c1 c2) = 
  fromMaybe (False, [], False) $ nestingFilter (b1 && b2, [cs1, cs2], h1 || h2) 
  where 
    (b1, cs1, h1) = eqCtxPat' p1 c1
    (b2, cs2, h2) = eqCtxPat' p2 c2

eqCtxPat' (PLet pbs pctx) (Let bs ctx) = 
  fromMaybe (False, [], False) $ nestingFilter (b1 && b2, [cs1, cs2], h1 || h2)
  where 
    (b1, cs1, h1) = eqBindPats' pbs bs
    (b2, cs2, h2) = eqCtxPat' pctx ctx

eqCtxPat' (PCase pctx pas) (Case ctx as) =  
  fromMaybe (False, [], False) $ nestingFilter (b1 && b2, [cs1, cs2], h1 || h2)
  where
    (b1, cs1, h1) = eqCtxPat' pctx ctx
    (b2, cs2, h2) = eqAltPats' pas as

eqCtxPat' (PAppD con1 pcs) (AppD con2 cs) 
  | length pcs == length cs && con1 == con2 =  
      fromMaybe (False, [], False) $ nestingFilter (and bs, ctxss, or hs)
    where (bs, ctxss, hs) = unzip3 $ fmap (uncurry eqCtxPat') (zip pcs cs)

-- This works just like a substituted hole.
eqCtxPat' (PCVar n1 (Just pctx) True) (CVar _ n2 (Just ctx))  
  | n1 == n2 = (b, ctx : cs, False) -- Set to False 
    where (b, cs, _) = eqCtxPat' pctx ctx 

-- This corresponds to a CVar whose substitution is not marked as nested,
-- so we recurse and remain in the same scope for empty hole occurrence.
eqCtxPat' (PCVar n1 (Just pctx) False) (CVar _ n2 (Just ctx)) 
  | n1 == n2 = eqCtxPat' pctx ctx

-- This is treated like an empty hole so matches any sub-context.                                          
eqCtxPat' (PCVar n1 Nothing _) (CVar _ n2 Nothing)     
  | n1 == n2 = (True, [], True)

eqCtxPat' (PCVar n1 Nothing _) (CVar _ n2 (Just ctx))  
  | n1 == n2 = (True, [ctx], False) -- Set to False

eqCtxPat' _ _ = (False, [], False) 

-- Context constructors: ------------------------------------------------------

eqConstPat :: CtxConstPat -> Ctx -> Bool 
eqConstPat P_VAR     Var{}    = True
eqConstPat P_LIT_INT LitInt{} = True
eqConstPat P_LIT_STR LitStr{} = True
eqConstPat P_ABS     Abs{}    = True
eqConstPat P_APP     App{}    = True
eqConstPat P_TICK    Tick{}   = True
eqConstPat P_LET     Let{}    = True
eqConstPat P_CASE    Case{}   = True
eqConstPat P_DATA    AppD{}   = True
-- LitInt/ListStr are built-in P_DATA
eqConstPat P_DATA    LitInt{} = True 
eqConstPat P_DATA    LitStr{} = True
eqConstPat P_LIST    ctx      = isList ctx
eqConstPat _         _        = False

-- Binds: ---------------------------------------------------------------------

eqBindPats :: [BindPat] -> [Bind] -> Bool 
eqBindPats pbs bs = let (b, _, _) = eqBindPats' pbs bs in b

eqBindPats' :: [BindPat] -> [Bind] -> (Bool, [Ctx], Bool)    
eqBindPats' pbs bs = 
  fromMaybe (False, [], False) $ nestingFilter (and ts, css, or hs)
  where  
    (ts, css, hs)              = unzip3 (go pbs bs)
    go [BindWildcard] bs       = [(True, [], hasHolesBinds bs)]
    go (pb : pbs)     (b : bs) = eqBindPat' pb b : go pbs bs
    go []             []       = [(True, [], False)]     
    go _              _        = [(False, [], False)]

eqBindPat :: BindPat -> Bind -> Bool 
eqBindPat (PBind ns1 pctx) (Bind ns2 ctx _) | ns1 == ns2 = eqCtxPat pctx ctx
eqBindPat _ _ = False  

eqBindPat' ::  BindPat -> Bind -> (Bool, [Ctx], Bool)
eqBindPat' (PBind ns1 pctx) (Bind ns2 ctx _) | ns1 == ns2 = eqCtxPat' pctx ctx
eqBindPat' _ _ = (False, [], False)                   

-- Alts: ----------------------------------------------------------------------

eqAltPats :: [AltPat] -> [Alt] -> Bool 
eqAltPats pas as = let (b, _, _) = eqAltPats' pas as in b

eqAltPats' :: [AltPat] -> [Alt] -> (Bool, [Ctx], Bool)
eqAltPats' pas as = 
  fromMaybe (False, [], False) $ nestingFilter (and ts, css, or hs)
  where
    (ts, css, hs)             = unzip3 (go pas as)
    go [AltWildcard] as       = [(True, [], hasHolesAlts as)]
    go (pa : pas)    (a : as) = eqAltPat' pa a : go pas as
    go []            []       = [(True, [], False)]     
    go _             _        = [(False, [], False)]

eqAltPat :: AltPat -> Alt -> Bool 
eqAltPat (PAlt con1 ns1 pctx) (Alt con2 ns2 ctx _) 
  | con1 == con2 && ns1 == ns2 = eqCtxPat pctx ctx
eqAltPat _ _ = False

eqAltPat'  :: AltPat -> Alt -> (Bool, [Ctx], Bool)
eqAltPat' (PAlt con1 ns1 pctx) (Alt con2 ns2 ctx _) 
  | con1 == con2 && ns1 == ns2 = eqCtxPat' pctx ctx
eqAltPat' _ _ = (False, [], False)

-------------------------------------------------------------------------------
-- Context pattern /generation/: --
-------------------------------------------------------------------------------
{- 
  - We use /lists/ of context patterns to generate context nestings from 
    contexts;
  - As before, we naively match on syntax and only care about syntactic 
    congruity; 
  - Each pattern in a list of context patterns matches against a 
    /sub-structure/ of the /previous/ substitution in the nesting, so we must 
    search through the sub-context looking for a match;
  - E.g., for a 'simple' list of patterns (such that the patterns themselves 
    aren't nested, but note that they can be if the user wishes),

      p_0, p_1, .., p_n

    and a context nesting, 

      T == C_0[C_1[C_2[..C_n[M]]]] for some term T:

    p_0 matches T and generates C_0[C_1],
    p_1 matches C_1 and generates C_1[C_2], 
    .. 
    p_n matches C_n and generates C_n[M].

    Thus overall we get C_0[C_1[C_2[..C_n[M]]]]. 

    (Note that T doesn't have to be a term, and we can generate nestings over
    contexts with empty holes too.)
-}

{- Top level function for context pattern generation: -------------------------

  Technical notes:
  ----------------
  - If the pattern p doesn't have an empty hole, then so long as it matches the 
    given context precisely (i.e., generates a context nesting /without/ a 
    substitution), we recurse on the remainder of the patterns ps in the list;
  - We split generation into two parts, a matching part that attempts to match 
    the given pattern against the /entire/ syntax of the (sub-)context in 
    question, and a searching part the recurses down the context's AST in order 
    to subsequently match on its sub-trees.
-}

-- Contexts: have to nub because matching and searching can lead to the same
-- result /independently/
ctxPatGen :: [CtxPat] -> Ctx -> [(Ctx, [Ctx], Bool)]
ctxPatGen [] ctx = [(ctx, [], CtxUtils.hasHoles ctx)]
ctxPatGen ps ctx = nub (ctxPatMatchRecGen ps ctx ++ ctxPatSearchGen ps ctx)
                          
-- Binds: ---------------------------------------------------------------------

bindPatsGen :: [CtxPat] -> [Bind] -> [([Bind], [Ctx], Bool)]
bindPatsGen ps = 
  patSeqGen . fmap (\b -> (b, [], hasHolesBind b) : bindPatGen ps b)
                             
bindPatGen :: [CtxPat] -> Bind -> [(Bind, [Ctx], Bool)]
bindPatGen ps (Bind ns ctx idx) = 
  fmap (\(c, cs, h) -> (Bind ns c idx, cs, h)) (ctxPatGen ps ctx) 

-- Alts: ----------------------------------------------------------------------

altPatsGen :: [CtxPat] -> [Alt] -> [([Alt], [Ctx], Bool)]
altPatsGen ps = 
  patSeqGen . fmap (\a -> (a, [], hasHolesAlt a) : altPatGen ps a)

altPatGen :: [CtxPat] -> Alt -> [(Alt, [Ctx], Bool)]
altPatGen ps (Alt con ns ctx idx) =  
  fmap (\(c, cs, h) -> (Alt con ns c idx, cs, h)) (ctxPatGen ps ctx) 

-- Helper for sequencing: -----------------------------------------------------

-- Note: must compute tail to avoid false matches                     
patSeqGen :: Eq a => [[(a, [Ctx], Bool)]] -> [([a], [Ctx], Bool)]
patSeqGen []  = []
patSeqGen xss = nub 
                  . catMaybes 
                  . fmap (nestingFilter 
                          . (\(xs, ctxss, hs) -> (xs, ctxss, or hs))
                          . unzip3) 
                  . tail 
                  . sequence
                  $ xss

-- Context searching: ---------------------------------------------------------
{- 
  - Here we recurse down the AST applying the generation algorithm at each
    sub-node in the tree;
  - Note that we rebuild the syntax to make sure any matches return a complete 
    definition.
-}
ctxPatSearchGen :: [CtxPat] -> Ctx -> [(Ctx, [Ctx], Bool)]

ctxPatSearchGen ps (Abs ns ctx) = 
  fmap (\(c, cs, h) -> (Abs ns c, cs, h)) (ctxPatGen ps ctx)

ctxPatSearchGen ps (App c1 c2)  = 
  fmap (\(c1' : c2' : _, cs, h) -> (App c1' c2', cs, h)) (patSeqGen [left, right])
  where 
    left  = (c1, [], CtxUtils.hasHoles c1) : ctxPatGen ps c1
    right = (c2, [], CtxUtils.hasHoles c2) : ctxPatGen ps c2

ctxPatSearchGen ps (Tick ctx) = fmap (\(c, cs, h) -> (Tick c, cs, h)) (ctxPatGen ps ctx)

ctxPatSearchGen ps (Let bs ctx) = 
  fmap (\(Left bs' : Right ctx' : _, cs, h) -> (Let bs' ctx', cs, h)) (patSeqGen [left, right])
  where 
    left  = (Left bs, [], hasHolesBinds bs) :
             ((\(bs, cs, h) -> (Left bs, cs, h)) <$> bindPatsGen ps bs)
    right = (Right ctx, [], CtxUtils.hasHoles ctx) :  
             ((\(c, cs, h)  -> (Right c, cs, h)) <$> ctxPatGen ps ctx)

ctxPatSearchGen ps (Case ctx as) = 
  fmap (\(Left ctx' : Right as' : _, cs, h) -> (Case ctx' as', cs, h)) (patSeqGen [left, right])
  where 
   left  = (Left ctx, [], CtxUtils.hasHoles ctx) : 
            ((\(c, cs, h)  -> (Left c, cs, h)) <$> ctxPatGen ps ctx)
   right = (Right as, [], hasHolesAlts as) :
            ((\(as, cs, h) -> (Right as, cs, h)) <$> altPatsGen ps as)

ctxPatSearchGen ps (AppD con ctxs) = 
  fmap (\(ctxs', cs, h) -> (AppD con ctxs', cs, h))
   . patSeqGen 
   . fmap (\c -> (c, [], CtxUtils.hasHoles c) : ctxPatGen ps c)
   $ ctxs
                                                                              
ctxPatSearchGen ps (CVar k ns  (Just ctx)) = 
  fmap (\(c, cs, h) -> (CVar k ns (Just c), cs, h)) (ctxPatGen ps ctx) 

ctxPatSearchGen _ _ = [] 

{- Context matching (for generation): -----------------------------------------
  - Here we match the first pattern p against the entire syntax of the context
    ctx and recurse, applying ctxPatGen to the remainder of the list ps and the
    inner substitution of the generated nesting r.
-}
ctxPatMatchRecGen :: [CtxPat] -> Ctx -> [(Ctx, [Ctx], Bool)]
ctxPatMatchRecGen []       ctx = [(ctx, [], CtxUtils.hasHoles ctx)]
ctxPatMatchRecGen (p : ps) ctx = maybe [] gen (ctxPatMatchGen p ctx)
 where gen (ctx, cs, _) = case cs of
        [] -> ctxPatGen ps ctx
        _  -> let (l, [r]) = splitAt (length cs - 1) cs
              in fmap (\(ctx', cs', h) -> (ctx, l ++ ctx' : cs', h)) (ctxPatGen ps r)

{- Version where inner nestings are matched without search

ctxPatMatchRecGen :: [CtxPat] -> Ctx -> [(Ctx, [Ctx], Bool)]
ctxPatMatchRecGen ps ctx =
  maybeToList (foldl match (Just (ctx, [], CtxUtils.hasHoles ctx)) ps)
  where match tup p = tup >>= \(ctx, cs, _) -> case cs of 
         [] -> ctxPatMatchGen p ctx 
         _  -> let (l, [r]) = splitAt (length cs - 1) cs
               in fmap (\(ctx', cs', h) -> (ctx, l ++ ctx' : cs', h)) 
                    (ctxPatMatchGen p r)
-}

{- 
  - The PatMatchGen functions below work in the same way as context pattern 
    checking, but they rebuild the syntax tree instead of returning a boolean 
    value.   
-}

-- Contexts: ------------------------------------------------------------------

ctxPatMatchGen :: CtxPat -> Ctx -> Maybe (Ctx, [Ctx], Bool)

ctxPatMatchGen Wildcard ctx = Just (ctx, [], CtxUtils.hasHoles ctx)
ctxPatMatchGen (PHole Nothing) Hole = Just (Hole, [], True)
ctxPatMatchGen (PHole Nothing) ctx = Just (Hole, [ctx], False) -- Set to False

ctxPatMatchGen (PVar ns1) (Var ns2)     | ns1 == ns2 = Just (Var ns2  , [], False)
ctxPatMatchGen (PLitInt i1) (LitInt i2) | i1  == i2  = Just (LitInt i2, [], False)
ctxPatMatchGen (PLitStr s1) (LitStr s2) | s1  == s2  = Just (LitStr s2, [], False)

ctxPatMatchGen (PHole (Just pctx)) ctx = 
  fmap (\(c, cs, _) -> (Hole, c : cs, False)) (ctxPatMatchGen pctx ctx) -- Set to False

ctxPatMatchGen (ConstPat pct) ctx
  | eqConstPat pct ctx = Just (ctx, [], CtxUtils.hasHoles ctx)

ctxPatMatchGen (PTick pctx) (Tick ctx) = 
  fmap (\(c, cs, h) -> (Tick c, cs, h)) (ctxPatMatchGen pctx ctx)  

ctxPatMatchGen (PAbs ns1 pctx) (Abs ns2 ctx)  
  | ns1 == ns2 = fmap (\(c, cs, h) -> (Abs ns2 c, cs, h)) (ctxPatMatchGen pctx ctx)  

ctxPatMatchGen (PApp p1 p2) (App c1 c2) = do 
  (c1', cs1, h1) <- ctxPatMatchGen p1 c1 
  (c2', cs2, h2) <- ctxPatMatchGen p2 c2 
  let h = h1 || h2
  cs <- validateCtxNestings h [cs1, cs2]
  Just (App c1' c2', cs, h)

ctxPatMatchGen (PLet pbs pctx) (Let bs ctx) = do 
  (bs',  cs1, h1) <- bindPatsMatchGen pbs bs 
  (ctx', cs2, h2) <- ctxPatMatchGen pctx ctx
  let h = h1 || h2
  cs <- validateCtxNestings h [cs1, cs2]
  Just (Let bs' ctx', cs, h)

ctxPatMatchGen (PCase pctx pas) (Case ctx as) = do 
  (ctx', cs1, h1) <- ctxPatMatchGen pctx ctx
  (as',  cs2, h2) <- altPatsMatchGen pas as 
  let h = h1 || h2
  cs <- validateCtxNestings h [cs1, cs2]
  Just (Case ctx' as', cs, h)

ctxPatMatchGen (PAppD con1 pcs) (AppD con2 cs) 
  | con1 == con2 && length pcs == length cs =  
     f >>= \(cs', css, hs) -> nestingFilter (AppD con2 cs', css, or hs)
    where f = fmap unzip3
               . sequence 
               . fmap (uncurry ctxPatMatchGen) 
               $ zip pcs cs

ctxPatMatchGen (PCVar _ (Just pctx) True) (CVar k ns (Just ctx)) =  
  fmap (\(c, cs, _) -> (CVar k ns Nothing, c : cs, False)) (ctxPatMatchGen pctx ctx) -- Set to False

ctxPatMatchGen (PCVar _ (Just pctx) False) (CVar k ns (Just ctx)) = 
  fmap (\(c, cs, h) -> (CVar k ns (Just c), cs, h)) (ctxPatMatchGen pctx ctx)

ctxPatMatchGen (PCVar _ Nothing _) (CVar k ns (Just ctx)) = 
  Just (CVar k ns Nothing, [ctx], False) -- Set to False

ctxPatMatchGen (PCVar _ Nothing _) (CVar k ns Nothing) = 
  Just (CVar k ns Nothing, [], True)

ctxPatMatchGen _ _ = Nothing 

-- Binds: ---------------------------------------------------------------------

bindPatsMatchGen :: [BindPat] -> [Bind] -> Maybe ([Bind], [Ctx], Bool)
bindPatsMatchGen pbs bs = 
  fmap unzip3 (go pbs bs) >>= \(bs', css, hs) -> nestingFilter (bs', css, or hs)
  where
    go [BindWildcard] bs       = let h = hasHolesBinds bs 
                                 in Just (fmap (\b -> (b, [], h)) bs)
    go (pb : pbs)     (b : bs) = (:) <$> bindPatMatchGen pb b <*> go pbs bs
    go []             []       = Just []
    go _              _        = Nothing

bindPatMatchGen :: BindPat -> Bind -> Maybe (Bind, [Ctx], Bool)
bindPatMatchGen (PBind ns1 pctx) (Bind ns2 ctx idx) 
  | ns1 == ns2 = fmap (\(c, cs, h) -> (Bind ns2 c idx, cs, h)) (ctxPatMatchGen pctx ctx) 
bindPatMatchGen _ _ = Nothing                                      
      
-- Alts: ----------------------------------------------------------------------

altPatsMatchGen :: [AltPat] -> [Alt] -> Maybe ([Alt], [Ctx], Bool)
altPatsMatchGen pas as = 
  fmap unzip3 (go pas as) >>= \(as', css, hs) -> nestingFilter (as', css, or hs)
  where
    go [AltWildcard] as       = let h = hasHolesAlts as 
                                in Just (fmap (\a -> (a, [], h)) as)
    go (pa : pas)    (a : as) = (:) <$> altPatMatchGen pa a <*> go pas as
    go []            []       = Just []
    go _             _        = Nothing

altPatMatchGen :: AltPat -> Alt -> Maybe (Alt, [Ctx], Bool)
altPatMatchGen (PAlt con1 ns1 pctx) (Alt con2 ns2 ctx idx) 
  | con1 == con2 && ns1 == ns2 = 
     fmap (\(c, cs, h) -> (Alt con2 ns2 c idx, cs, h)) (ctxPatMatchGen pctx ctx) 
altPatMatchGen _ _ = Nothing

-------------------------------------------------------------------------------
-- Derived transformations: --
-------------------------------------------------------------------------------

-- Find all paths to a CtxPat.
ctxPatPathsT :: ( MonadCatch m, ReadPath c Crumb 
                , ExtendPath c Crumb
                , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                , AddBinders (ExtendContext c (LocalPath Crumb)) )
                => CtxPat -> Transform c m U [LocalPath Crumb]
ctxPatPathsT  = pathsToT . promoteT . liftT . eqCtxPat


{-

Note: /if/ we add these back, generalise from T.

-- Find all paths to a UPat in a U
uPatPathsT               :: UPat -> T U [LocalPath Crumb]
uPatPathsT (UPDef pdef)   = defPatPathsT pdef
uPatPathsT (UPCtx pctx)   = ctxPatPathsT pctx 
uPatPathsT (UPBind pbind) = bindPatPathsT pbind 
uPatPathsT (UPAlt palt)   = altPatPathsT palt

uPatPaths                :: Injection a U => UPat -> a -> TM [LocalPath Crumb]
uPatPaths pu x            = applyT (uPatPathsT pu) emptyKureContext (injectU x)
                                                       
-- Find all paths to a DefPat in a U
defPatPathsT             :: DefPat -> T U [LocalPath Crumb]
defPatPathsT              = pathsToT . promoteT . liftT . eqDefPat

defPatPaths              :: Injection a U => DefPat -> a -> TM [LocalPath Crumb]
defPatPaths pdef x        = applyT (defPatPathsT pdef) emptyKureContext (injectU x)

-- Find all paths to a CtxPat in a U
ctxPatPathsT             :: CtxPat -> T U [LocalPath Crumb]
ctxPatPathsT              = pathsToT . promoteT . liftT . eqCtxPat 

ctxPatPaths              :: Injection a U => CtxPat -> a -> TM [LocalPath Crumb]
ctxPatPaths pctx x        = applyT (ctxPatPathsT pctx) emptyKureContext (injectU x)

-- Find all paths to a BindPat in a U
bindPatPathsT            :: BindPat -> T U [LocalPath Crumb]
bindPatPathsT             = pathsToT . promoteT . liftT . eqBindPat 

bindPatPaths             :: Injection a U => BindPat -> a -> TM [LocalPath Crumb]
bindPatPaths pbind x      = applyT (bindPatPathsT pbind) emptyKureContext (injectU x)

-- Find all paths to an AltPat in a U
altPatPathsT             :: AltPat -> T U [LocalPath Crumb]
altPatPathsT              = pathsToT . promoteT . liftT . eqAltPat

altPatPaths              :: Injection a U => AltPat -> a -> TM [LocalPath Crumb]
altPatPaths palt x        = applyT (altPatPathsT palt) emptyKureContext (injectU x) 

-}