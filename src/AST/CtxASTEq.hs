{-# LANGUAGE FlexibleContexts #-}

module CtxASTEq 
 ( alphaEqPathsT  -- Find paths to alpha-equivalent injectables.
 , alphaEqT       -- Alpha-equiv. lifted to a transformation.
 , altAlphaEq     -- Alpha-equiv. of case alternatives.
 , bindAlphaEq    -- Alpha-equiv. of let bindings.
 , ctxAlphaEq     -- Alpha-equiv. of contexts.
 , gBindAlphaEq   -- Alpha-equiv. of global bindings.
 , uAlphaEq       -- Alpha-equiv. of Us.
 ) where 

import Classes       (AddBinders, SafeNames, freshVar)
import Crumb         (Crumb)
import CtxAST        (Alt(..), Bind(..), Ctx(..), GBind(..))
import CtxKind       ()
import CtxUtils      (bindBinder, reindexBinds)
import KureContext   (emptyKureContext)
import Normalisation (renameMultipleR)
import Subst         (sieveListT, substTryListVarR, substTryVarR)
import Universes     (U(..))
import Utils         (combinations)

import Control.Monad                   ((>=>), join)
import Control.Monad.Loops             (allM, anyM)
import Control.Monad.State             (MonadState)
import Language.KURE.ExtendableContext (ExtendContext)
import Language.KURE.Pathfinder        (pathsToT)

import Language.KURE    
  ( ExtendPath
  , Injection
  , LocalPath
  , MonadCatch
  , ReadPath
  , Transform
  , applyR
  , applyT
  , contextfreeT
  , inject
  )

{-
  Information:
  -----------------------------------------------------------------------------
  - Alpha equivalence (i.e., equivalent up to renaming of bound variables) on 
    the abstract syntax of the source language;
  - Also a number of derived transformations from this.
-}

-- U equivalence up to renaming of bound variables.
uAlphaEq :: (MonadCatch m, SafeNames s, MonadState s m) => U -> U -> m Bool
uAlphaEq (UGBind g1) (UGBind g2) = gBindAlphaEq g1 g2 
uAlphaEq (UCtx c1)   (UCtx c2)   = ctxAlphaEq c1 c2
uAlphaEq (UBind b1)  (UBind b2)  = bindAlphaEq b1 b2
uAlphaEq (UAlt a1)   (UAlt a2)   = altAlphaEq a1 a2
uAlphaEq _           _           = return False


-- GBind equivalence up to renaming of bound variables.
gBindAlphaEq :: ( MonadCatch m, SafeNames s
                , MonadState s m ) 
                => GBind -> GBind -> m Bool 
gBindAlphaEq (CBind k1 nss1 c1) (CBind k2 nss2 c2) 
  | k1 == k2 && nss1 == nss2 = ctxAlphaEq c1 c2 
gBindAlphaEq (TBind nss1 t1)    (TBind nss2 t2)     
  | nss1 == nss2 = ctxAlphaEq t1 t2 
gBindAlphaEq _ _ = return False


-- Ctx equivalence up to renaming of bound variables;
-- The order of let bindings doesn't matter;
-- The order of case alternative does.
ctxAlphaEq :: ( MonadCatch m, SafeNames s
              , MonadState s m ) 
              => Ctx -> Ctx -> m Bool


-- For basic constructors, just check if atoms match. 
ctxAlphaEq (Var ns1)    (Var ns2)   = return (ns1 == ns2)
ctxAlphaEq (LitInt i1)  (LitInt i2) = return (i1 == i2)
ctxAlphaEq (LitStr s1)  (LitStr s2) = return (s1 == s2)
ctxAlphaEq Hole         Hole        = return True 
ctxAlphaEq (Tick c1)    (Tick c2)   = ctxAlphaEq c1 c2

-- CVar's => context kinds, names and substs. must match.
ctxAlphaEq (CVar k1 ns1 Nothing) (CVar k2 ns2 Nothing) = return (k1 == k2 
                                                             && ns1 == ns2)

ctxAlphaEq (CVar k1 ns1 (Just c1)) (CVar k2 ns2 (Just c2)) =  
  (&&) (k1 == k2 && ns1 == ns2) <$> ctxAlphaEq c1 c2 

-- For semantics, order of alts. /does/ matter, so we don't check all 
-- combinations (cf. let bindings);
-- We check if alts are pairwise alpha-equiv and the scruts too.
ctxAlphaEq (Case c1 as1) (Case c2 as2) =  
  (&&) <$> ctxAlphaEq c1 c2 <*> allM (uncurry altAlphaEq) (zip as1 as2)

-- Datatypes have to be pairwise alpha-equiv.
ctxAlphaEq (AppD con1 cs1) (AppD con2 cs2) 
  | con1 == con2 && length cs1 == length cs2 = 
      allM (uncurry ctxAlphaEq) (zip cs1 cs2)

--  App => check each child pairwise.
ctxAlphaEq (App c1 c2) (App c1' c2') =  
  (&&) <$> ctxAlphaEq c1 c1' <*> ctxAlphaEq c2 c2'  

-- Abs/Let slightly more involved: --------------------------------------------

-- Abs => gen new binder and subst. in bodies, then check.
ctxAlphaEq (Abs ns1 c1) (Abs ns2 c2) 
  -- If binders equal, then just check bodies.
  | ns1 == ns2 = ctxAlphaEq c1 c2
  -- Generate new unique binder, subst. for old binders and check alpha-eq. of 
  -- both bodies.
  | otherwise  = (applyT sieveListT emptyKureContext >=> freshVar) [c1, c2] 
                  >>= \bs -> join $ 
                  ctxAlphaEq <$> applyR (substTryVarR ns1 bs)
                                  emptyKureContext c1
                             <*> applyR (substTryVarR ns2 bs) 
                                  emptyKureContext c2  

-- Order of let bindings /doesn't/ matter, so we take all combinations
-- and then rename them to have the same binders, check the alpha-eq. of 
-- each pairwise bindings and the let bodies.
ctxAlphaEq l1@(Let bs1 c1) l2@(Let bs2 c2)

  | length bs1 == length bs2 = do 

      -- Get all used variables and generate fresh binders.
      invalid <- applyT sieveListT emptyKureContext [l1, l2]
      bss <- mapM (const $ Just <$> freshVar invalid) bs1

      {- 
        Take all combinations of bs1 and bs2 like so:
          
          bs1 = [a = 1, b = 2] 
          bs2 = [x = 3, y = 4]
         
          combine => [ ([a = 1, b = 2], [x = 3, y = 4])
                     , ([b = 2, a = 1], [x = 3, y = 4])
 
          - Now we check if when (a, x) and (b, y) are renamed to the same
            (distinct) binders, the binding bodies and let body is alpha-eq.
          - If not, we check when (b, x) and (a, y) are ranmed to the same
            binders.. etc.
      -} 
      lss <- mapM (rename bss) (fmap unzip $ combinations bs1 bs2) 
      anyM (uncurry letEq) lss

    where 
     
     -- Rename each pair of bindings to have the same binder names according
     -- to nss generated above, then we can check binding alpha-eq.
     -- and let body alpha-eq.
     rename bss (bs1, bs2) = 
       (,) <$> applyR (renameMultipleR $ zip (fmap bindBinder bs1) bss) 
                 emptyKureContext (Let (reindexBinds bs1) c1)
           <*> applyR (renameMultipleR $ zip (fmap bindBinder bs2) bss) 
                 emptyKureContext (Let (reindexBinds bs2) c2)
     
     -- Check let alpha-eq. by checking pairwise bindings and let bodies.
     letEq (Let bs1 c1) (Let bs2 c2) = 
       (&&) <$> allM (uncurry bindEq) (zip bs1 bs2) 
            <*> ctxAlphaEq c1 c2
     letEq _ _ = return False -- Silence warning.
     
     -- Binding alpha-eq. ignored indices. 
     bindEq (Bind ns1 c1 _) (Bind ns2 c2 _) = 
       (&&) (ns1 == ns2) <$> ctxAlphaEq c1 c2

ctxAlphaEq _ _ = return False


-- Bind equivalence up to renaming of bound variables;
-- Here we just rename the binders as per an abs.
bindAlphaEq :: ( MonadCatch m, SafeNames s
               , MonadState s m ) 
               => Bind -> Bind -> m Bool
bindAlphaEq b1@(Bind ns1 c1 _) b2@(Bind ns2 c2 _) = 
  (applyT sieveListT emptyKureContext >=> freshVar) [b1, b2] >>= \ns -> 
    join $ ctxAlphaEq <$> applyR (substTryVarR ns1 ns) emptyKureContext c1
                      <*> applyR (substTryVarR ns2 ns) emptyKureContext c2


-- Alt equivalence up to renaming of bound variables;
-- Rename all binders again.                                                                              
altAlphaEq :: ( MonadCatch m, SafeNames s
              , MonadState s m ) 
               => Alt -> Alt -> m Bool                                            
altAlphaEq a1@(Alt con1 nss1 c1 _) a2@(Alt con2 nss2 c2 _)
  | con1 == con2 && length nss1 == length nss2 = do 
      invalid <- applyT sieveListT emptyKureContext [a1, a2]
      nss     <- mapM (const $ freshVar invalid) nss1
      join $ ctxAlphaEq <$> applyR (substTryListVarR $ zip nss1 nss) 
                              emptyKureContext c1
                        <*> applyR (substTryListVarR $ zip nss2 nss) 
                             emptyKureContext c2  
  | otherwise = return False         


-- Derived transformations : --------------------------------------------------

-- Lift alpha-eq. to a transformation.
alphaEqT :: (MonadCatch m, MonadState s m
            , SafeNames s, Injection a U) 
            => a -> Transform c m U Bool 
alphaEqT x = contextfreeT $ \u -> inject x `uAlphaEq` u

-- Find all paths using alpha-equiv
alphaEqPathsT :: ( MonadCatch m, MonadState s m
                 , SafeNames s, Injection a U
                 , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                 , AddBinders (ExtendContext c (LocalPath Crumb)) 
                 , ExtendPath c Crumb ) 
                 => a -> Transform c m U [LocalPath Crumb]
alphaEqPathsT  = pathsToT . alphaEqT