{-# LANGUAGE FlexibleContexts #-}

module CtxGen where 

import CtxAST -- Ctx, Name, Bind etc.
import CtxPatAST (CtxPat(..), CtxConstPat(..))
import CtxEqLib (CtxEqLib(..))
import CtxKind (CtxKind(..))
import KureMonad (TM, T, R, KureMEnv)
import KureContext (KureContext, emptyKureContext)
import CtxCheck (isStdCtx, autoAppCtxBinderSubst, autoAppBinderChain)
import CtxPatMatch (ctxPatMatchGen, patSeqGen, eqCtxPat)
import CtxUtils -- all 
import TransUtils ( freeVars, freeVarsT, boundVarsHolesNoLetBindersT 
                  , freeVarsList, boundVarsHolesListT, boundVarsHolesT )
import Classes 
import Utils ((.*), powerset, replaceAtIdx, concatMapM)

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Maybe (catMaybes)
import Data.List (intersect)
import Language.KURE ( ifM, rewrite, applyT
                     , contextfreeT, transform )

-- import Control.Monad.Extra (allM)

{-
  <TO-DO>:

  Information:
  -----------------------------------------------------------------------------
  - <TO-DO>

  Working notes:
  -----------------------------------------------------------------------------
  - I've commented out all standard context checks because as things are
    implemented right now, they'll always be /true/
-}

-------------------------------------------------------------------------------
-- Top-level functions for context generation: --
-------------------------------------------------------------------------------

genCtxs :: (Maybe Ctx -> Bool) 
           -> [CtxKind] 
           -> CtxEqLib 
           -> Ctx 
           -> [(Ctx, [Ctx], Bool)] 
genCtxs f ks lib ctx = runReader (genNestFun f ks ctx) lib
  








genCtxsSpecFreeSubstCurrScope :: (Maybe Ctx -> Bool) 
                                 -> T Ctx [Name]
                                 -> [CtxKind] 
                                 -> CtxEqLib 
                                 -> T Ctx [(Ctx, [Ctx], Bool)] 
genCtxsSpecFreeSubstCurrScope f g ks lib = 
  transform $ \c ctx -> (specFreeVarsCtxSubsts g) c (genCtxs f ks lib ctx)

genCtxsSpecFreeSubstNewScope :: (Maybe Ctx -> Bool) 
                                -> T Ctx [Name]
                                -> [CtxKind] 
                                -> CtxEqLib 
                                -> T Ctx [(Ctx, [Ctx], Bool)] 
genCtxsSpecFreeSubstNewScope f g ks lib = 
  contextfreeT $ \ctx -> (specFreeVarsCtxSubsts g) emptyKureContext (genCtxs f ks lib ctx)

       

genCtxsSpecFreeCtxCurrScope :: (Maybe Ctx -> Bool) 
                               -> [CtxKind] 
                               -> CtxEqLib 
                               -> T Ctx [(Ctx, [Ctx], Bool)] 
genCtxsSpecFreeCtxCurrScope f ks lib =
  transform $ \c ctx -> (freeVarsCtxs c) (genCtxs f ks lib ctx)


-- Generate contexts whose free variables are not captured in the current scope.
genFreeCtxsCurrScope :: (Maybe Ctx -> Bool) 
                        -> [CtxKind] 
                        -> CtxEqLib 
                        -> T Ctx [(Ctx, [Ctx], Bool)] 
genFreeCtxsCurrScope f ks lib = 
  transform $ \c ctx -> (freeVarsCtxs c) (genCtxs f ks lib ctx)


-- Context nesting checks
genSpecFreeVarsCtxNestSubstsNewScope :: (Maybe Ctx -> Bool) 
                                -> T Ctx [Name]
                                -> [CtxKind] 
                                -> CtxEqLib 
                                -> T Ctx [(Ctx, Ctx, Term)] 
genSpecFreeVarsCtxNestSubstsNewScope f g ks lib =
  contextfreeT $ \ctx -> specFreeVarsCtxNestSubsts g emptyKureContext (genCtxs f ks lib ctx)


genSpecFreeVarsCtxNestSubstsCurrScope :: (Maybe Ctx -> Bool) 
                                -> T Ctx [Name]
                                -> [CtxKind] 
                                -> CtxEqLib 
                                -> T Ctx [(Ctx, Ctx, Term)] 
genSpecFreeVarsCtxNestSubstsCurrScope f g ks lib =
  transform $ \c ctx -> specFreeVarsCtxNestSubsts g c (genCtxs f ks lib ctx)



-- Generation helpers: --           

specSubstGen              ::  Ctx -> Maybe Ctx -> Bool
specSubstGen c1 (Just c2)  =  c1 == c2
specSubstGen _  _          =  False 

specSubtGenEmpty              ::  Ctx -> Maybe Ctx -> Bool 
specSubtGenEmpty c1 (Just c2)  =  c1 == c2 
specSubtGenEmpty _ _           =  True 

specPatSubstGen                ::  CtxConstPat -> Maybe Ctx -> Bool 
specPatSubstGen pct (Just ctx)  =  eqCtxPat (ConstPat pct) ctx 
specPatSubstGen _ _             =  False

termOnlySubst :: Maybe Ctx -> Bool 
termOnlySubst  = maybe False isTerm

varOnlySubst :: Maybe Ctx -> Bool 
varOnlySubst = maybe False isVar

genPairs :: [(Ctx, [Ctx], Bool)]  -> [(Ctx, Term)]
genPairs  = catMaybes . fmap pairs
            where 
                 pairs ((ctx, [sub], _)) = Just (ctx, sub)
                 pairs _ = Nothing

genTrips :: [(Ctx, [Ctx], Bool)]  -> [(Ctx, Ctx, Term)]
genTrips  = catMaybes . fmap trips
            where 
                 trips ((ctx, [sub1, sub2], _)) = Just (ctx, sub1, sub2)
                 trips _ = Nothing

genQuads :: [(Ctx, [Ctx], Bool)]  -> [(Ctx, Ctx, Ctx, Term)]
genQuads  = catMaybes . fmap quads
            where 
                 quads ((ctx, [sub1, sub2, sub3], _)) = Just (ctx, sub1, sub2, sub3)
                 quads _ = Nothing

-------------------------------------------------------------------------------
-- Automated context generation: --
-------------------------------------------------------------------------------

autoStdCtxs        ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
autoStdCtxs f Hole  |  f Nothing    = return [(Hole, [], True)]
                    |  otherwise    = return []
autoStdCtxs f ctx   |  f (Just ctx) = ((Hole, [ctx], False) :) <$> rest  -- Set to False
                    |  otherwise    = rest 
                       where rest = (++) <$> autoStdCtxs' f ctx <*> autoEqCtxs (pruneCtxSubst f) std ctx

autoStdCtxs'                    ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
autoStdCtxs' f (Abs ns ctx)      =  fmap (\(c, cs, h) -> (Abs ns c, cs, h)) <$> autoStdCtxs f ctx
-- autoStdCtxs' f (App ctx v@Var{})  =  fmap (\(c, cs, h) -> (App c v, cs, h))  <$> autoStdCtxs f ctx
-- C1 C2 is a syntactic sugar for let x = C2 in C1 x
-- Hence it is valid so long as C1/C2 are valid standard contexts
-- We generate over both arguments and effectively treat it as a let
-- statement with one binding
autoStdCtxs' f (App c1 c2)       =  fmap (\(c1' : c2' : _, cs, h) -> (App c1' c2', cs, h)) 
                                     .* seqGen
                                     <$> left <*> right
                                    where 
                                         {-left  = ifM (isStdCtx c1) (((c1, [], hasHoles c1) :) <$> rest) rest  
                                                 where rest = autoStdCtxs f c1
                                         right = ifM (isStdCtx c2) (((c2, [], hasHoles c2) :) <$> rest) rest  
                                                 where rest = autoStdCtxs f c2-}
                                         left  = ((c1, [], hasHoles c1) :) <$> autoStdCtxs f c1
                                         right = ((c2, [], hasHoles c2) :) <$> autoStdCtxs f c2
-- Tick C is a syntactic sugar for let x = C in x
-- Hence it is valid so long as C is a valid standard context      
autoStdCtxs' f (Tick ctx)        =  fmap (\(c, cs, h) -> (Tick c, cs, h)) <$> autoStdCtxs f ctx                                                
-- For lets/cases we have to make sure that contexts with multiple 
-- holes have the same substitutions
autoStdCtxs' f (Let bs ctx)      =  fmap (\(Left bs' : Right ctx' : _, cs, h) -> (Let bs' ctx', cs, h)) 
                                     .* seqGen
                                     <$> left <*> right
                                    where
                                         {-left  = ifM (allM (\(Bind _ ctx _) -> isStdCtx ctx) bs) (((Left bs, [], hasHolesBinds bs) :) <$> rest) rest
                                                 where rest = fmap (\(bs, cs, h) -> (Left bs, cs, h)) <$> genBindsStdCtxs (autoStdCtxs f) bs
                                         right = ifM (isStdCtx ctx) (((Right ctx, [], hasHoles ctx) :) <$> rest) rest 
                                                 where rest = fmap (\(c, cs, h) -> (Right c, cs, h)) <$> autoStdCtxs f ctx-}
                                         left  = ((Left bs, [], hasHolesBinds bs) :) <$> (fmap (\(bs, cs, h) -> (Left bs, cs, h)) <$> genBindsStdCtxs (autoStdCtxs f) bs)
                                         right = ((Right ctx, [], hasHoles ctx) :) <$> (fmap (\(c, cs, h) -> (Right c, cs, h)) <$> autoStdCtxs f ctx)
autoStdCtxs' f (Case ctx as)     =  fmap (\(Left ctx' : Right as' : _, cs, h) -> (Case ctx' as', cs, h))
                                     .* seqGen
                                     <$> left <*> right
                                    where 
                                         {-left  = ifM (isStdCtx ctx) (((Left ctx, [], hasHoles ctx) :) <$> rest) rest 
                                                 where rest = fmap (\(c, cs, h) -> (Left c, cs, h)) <$> autoStdCtxs f ctx
                                         right = ifM (allM (\(Alt _ _ ctx _) -> isStdCtx ctx) as) (((Right as, [], hasHolesAlts as) :) <$> rest) rest
                                                 where rest = fmap (\(as, cs, h) -> (Right as, cs, h)) <$> genAltsStdCtxs (autoStdCtxs f) as-}
                                         left  = ((Left ctx, [], hasHoles ctx) :) <$> (fmap (\(c, cs, h) -> (Left c, cs, h)) <$> autoStdCtxs f ctx)
                                         right = ((Right as, [], hasHolesAlts as) :) <$> (fmap (\(as, cs, h) -> (Right as, cs, h)) <$> genAltsStdCtxs (autoStdCtxs f) as)
autoStdCtxs' f ctx@CVar{}        = unrollCVarGen f ctx 
autoStdCtxs' _ _                 = return []
{-                                         
autoStdCtxs' f (CVar k ns Nothing)     |  f Nothing    = return [(CVar k ns Nothing, [], True)]   
autoStdCtxs' f (CVar k ns (Just ctx))  |  f (Just ctx) = return [(CVar k ns Nothing, [ctx], False)]  -- Set to False   
-}



-- Value contexts
autoValCtxs       ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
autoValCtxs f ctx  =  (++) <$> autoValCtxs' f ctx <*> autoEqCtxs (pruneCtxSubst f) val ctx

autoValCtxs'                            ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
autoValCtxs' f (Abs ns ctx)              =  fmap (\(c, cs, h) -> (Abs ns c, cs, h)) <$> autoStdCtxs f ctx
autoValCtxs' f ctx@CVar{}                = unrollCVarGen f ctx 
--autoValCtxs' f (CVar VAL ns Nothing)     |  f Nothing    = return [(CVar VAL ns Nothing, [], True)]
--autoValCtxs' f (CVar VAL ns (Just ctx))  |  f (Just ctx) = return [(CVar VAL ns Nothing, [ctx], False)] --  Set to False
autoValCtxs' _ _                         =  return []

-- Evaluation contexts
autoEvalCtxs       ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
autoEvalCtxs f ctx  =  (++) <$> autoEvalCtxs' f ctx <*> ((++) <$> autoAppCtxs f ctx <*> autoEqCtxs (pruneCtxSubst f) eval ctx) 

autoEvalCtxs'                             ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
-- A M, M /= var, is a syntactic sugar for let x = M in A x
-- Hence it is valid so long as M is a valid term and A is a valid applicative context
autoEvalCtxs' f (App c1 c2)                |  isTerm c2 && not (isVar c2) = fmap (\(c, cs, h) -> (App c c2, cs, h)) <$> autoAppCtxs f c1
-- Tick A is a syntactic sugar for let x = A in x
-- Hence it is a valid so long as A is a valid applicative context
autoEvalCtxs' f (Tick ctx)                 =  fmap (\(c, cs, h) -> (Tick c, cs, h)) <$> autoAppCtxs f ctx
autoEvalCtxs' f tel@(Let bs ctx)           |  all (\(Bind _ ctx _) -> isTerm ctx) bs = 
                                              -- let { x_ = M_ } in A
                                              (++) <$> (fmap (\(c, cs, h) -> (Let bs c, cs, h)) <$> autoAppCtxs f ctx) <*> rest 
                                           |  otherwise = rest 
                                              -- let { y = M, x_0 = A_0[x_1], x_1 = A_1[x_2], .., x_n = A_n } in A[x_0]
                                              where rest = genLetEvalCtxs (autoAppCtxs f) tel
autoEvalCtxs' f ctx@CVar{}                 = unrollCVarGen f ctx                                             
--autoEvalCtxs' f (CVar EVAL ns Nothing)     |  f Nothing    = return [(CVar EVAL ns Nothing, [], True)] 
--autoEvalCtxs' f (CVar EVAL ns (Just ctx))  |  f (Just ctx) = return [(CVar EVAL ns Nothing, [ctx], False)] -- Set to False                                                            
autoEvalCtxs' _ _                          =  return []

-- Applicative contexts
autoAppCtxs        ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
autoAppCtxs f Hole  |  f Nothing    = return [(Hole, [], True)]
                    |  otherwise    = return []
autoAppCtxs f ctx   |  f (Just ctx) = ((Hole, [ctx], False) :) <$> rest  -- Set to False
                    |  otherwise    = rest 
                       where rest = (++) <$> autoAppCtxs' f ctx <*> autoEqCtxs (pruneCtxSubst f) app ctx                                

autoAppCtxs'                            ::  (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
-- The RHS of this application must be a variable otherwise it's not an applicative context
autoAppCtxs' f (App ctx v@Var{})         =  fmap (\(c, cs, h) -> (App c v, cs, h)) <$> autoAppCtxs f ctx
autoAppCtxs' f (Case ctx as)             |  all (\(Alt _ _ ctx _) -> isTerm ctx) as  
                                         =  fmap (\(c, cs, h) -> (Case c as, cs, h)) <$> autoAppCtxs f ctx 

autoAppCtxs' f ctx@CVar{}                = unrollCVarGen f ctx  
--autoAppCtxs' f (CVar APP ns Nothing)     |  f Nothing    = return [(CVar APP ns Nothing, [], True)]                                                                                                                                                                                                                                                                           
--autoAppCtxs' f (CVar APP ns (Just ctx))  |  f (Just ctx) = return [(CVar APP ns Nothing, [ctx], False)] -- Set to False
autoAppCtxs' _ _                         =  return []

-------------------------------------------------------------------------------
-- Guided context generation: --
-------------------------------------------------------------------------------

-- Standard contexts
stdCtxNest       ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
stdCtxNest f ctx  =  (++) <$> (fmap (\(c, cs, h) -> (Hole, c : cs, h)) <$> f ctx) 
                          <*> ((++) <$> stdCtxNest' f ctx <*> autoEqCtxs (nestCtxSubst f) std ctx)

stdCtxNest'                     ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
stdCtxNest' f (Abs ns ctx)       =  fmap (\(c, cs, h) -> (Abs ns c, cs, h)) <$> stdCtxNest f ctx
-- stdCtxNest' f (App ctx v@Var{})  =  fmap (\(c, cs, h) -> (App c v, cs, h))  <$> stdCtxNest f ctx
stdCtxNest' f (App c1 c2)        =  fmap (\(c1' : c2' : _, cs, h) -> (App c1' c2', cs, h)) 
                                     .* seqGen
                                     <$> left <*> right
                                    where 
                                         {-left  = ifM (isStdCtx c1) (((c1, [], hasHoles c1) :) <$> rest) rest
                                                 where rest = stdCtxNest f c1
                                         right = ifM (isStdCtx c2) (((c2, [], hasHoles c2) :) <$> rest) rest  
                                                 where rest = stdCtxNest f c2-}
                                         left  = ((c1, [], hasHoles c1) :) <$> stdCtxNest f c1
                                         right = ((c2, [], hasHoles c2) :) <$> stdCtxNest f c2

stdCtxNest' f (Tick ctx)         =  fmap (\(c, cs, h) -> (Tick c, cs, h)) <$> stdCtxNest f ctx 
stdCtxNest' f (Let bs ctx)       =  fmap (\(Left bs' : Right ctx' : _, cs, h) -> (Let bs' ctx', cs, h)) 
                                     .* seqGen
                                     <$> left <*> right
                                    where
                                         {-left  = ifM (allM (\(Bind _ ctx _) -> isStdCtx ctx) bs) (((Left bs, [], hasHolesBinds bs) :) <$> rest) rest
                                                 where rest = fmap (\(bs, cs, h) -> (Left bs, cs, h)) <$> genBindsStdCtxs (stdCtxNest f) bs
                                         right = ifM (isStdCtx ctx) (((Right ctx, [], hasHoles ctx) :) <$> rest) rest 
                                                 where rest = fmap (\(c, cs, h) -> (Right c, cs, h)) <$> stdCtxNest f ctx-}
                                         left  = ((Left bs, [], hasHolesBinds bs) :) <$> (fmap (\(bs, cs, h) -> (Left bs, cs, h)) <$> genBindsStdCtxs (stdCtxNest f) bs)
                                         right = ((Right ctx, [], hasHoles ctx) :) <$> (fmap (\(c, cs, h) -> (Right c, cs, h)) <$> stdCtxNest f ctx)
stdCtxNest' f (Case ctx as)     =   fmap (\(Left ctx' : Right as' : _, cs, h) -> (Case ctx' as', cs, h))
                                     .* seqGen
                                     <$> left <*> right
                                    where 
                                         {-left  = ifM (isStdCtx ctx) (((Left ctx, [], hasHoles ctx) :) <$> rest) rest 
                                                 where rest = fmap (\(c, cs, h) -> (Left c, cs, h)) <$> stdCtxNest f ctx
                                         right = ifM (allM (\(Alt _ _ ctx _) -> isStdCtx ctx) as) (((Right as, [], hasHolesAlts as) :) <$> rest) rest
                                                 where rest = fmap (\(as, cs, h) -> (Right as, cs, h)) <$> genAltsStdCtxs (stdCtxNest f) as-}
                                         left  = ((Left ctx, [], hasHoles ctx) :) <$> (fmap (\(c, cs, h) -> (Left c, cs, h)) <$> stdCtxNest f ctx)
                                         right = ((Right as, [], hasHolesAlts as) :) <$> (fmap (\(as, cs, h) -> (Right as, cs, h)) <$> genAltsStdCtxs (stdCtxNest f) as)
                                                 
stdCtxNest' f (CVar k ns (Just ctx))  =  fmap (\(c, cs, h) -> (CVar k ns Nothing, c : cs, h)) <$> f ctx
stdCtxNest' _ _                       =  return []

-- Value contexts
valCtxNest       ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
valCtxNest f ctx  =  (++) <$> valCtxNest' f ctx <*> autoEqCtxs (nestCtxSubst f) val ctx

valCtxNest'                            ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
valCtxNest' f (Abs ns ctx)              =  fmap (\(c, cs, h) -> (Abs ns c, cs, h)) <$> stdCtxNest f ctx  
valCtxNest' f (CVar VAL ns (Just ctx))  =  fmap (\(c, cs, h) -> (CVar VAL ns Nothing, c : cs, h)) <$> f ctx
valCtxNest' _ _                         =  return []

-- Evaluation contexts
evalCtxNest       ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
evalCtxNest f ctx  =  (++) <$> evalCtxNest' f ctx 
                           <*> ((++) <$> appCtxNest' f ctx <*> autoEqCtxs (nestCtxSubst f) eval ctx)
                                                                
evalCtxNest'                             ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
evalCtxNest' f (App c1 c2)                |  isTerm c2 && not (isVar c2) = fmap (\(c, cs, h) -> (App c c2, cs, h)) <$> appCtxNest f c1
evalCtxNest' f (Tick ctx)                 =  fmap (\(c, cs, h) -> (Tick c, cs, h)) <$> appCtxNest f ctx                          
evalCtxNest' f tel@(Let bs ctx)           |  all (\(Bind _ ctx _) -> isTerm ctx) bs = 
                                             (++) <$> (fmap (\(c, cs, h) -> (Let bs c, cs, h)) <$> appCtxNest f ctx) <*> rest 
                                          |  otherwise = rest 
                                             where rest = genLetEvalCtxs (appCtxNest f) tel
evalCtxNest' f (CVar EVAL ns (Just ctx))  =  fmap (\(c, cs, h) -> (CVar EVAL ns Nothing, c : cs, h)) <$> f ctx                                                            
evalCtxNest' _ _                          =  return []

-- Applicative contexts
appCtxNest       ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
appCtxNest f ctx  =  (++) <$> (fmap (\(c, cs, h) -> (Hole, c : cs, h)) <$> f ctx)
                          <*> ((++) <$> appCtxNest' f ctx <*> autoEqCtxs (nestCtxSubst f) app ctx)
                                                      
appCtxNest'                            ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
appCtxNest' f (App ctx v@Var{})         =  fmap (\(c, cs, h) -> (App c v, cs, h)) <$> appCtxNest f ctx
appCtxNest' f (Case ctx as)             |  all (\(Alt _ _ ctx _) -> isTerm ctx) as           
                                        =  fmap (\(c, cs, h) -> (Case c as, cs, h)) <$> appCtxNest f ctx                                                                                                                                                                                                                                                              
appCtxNest' f (CVar APP ns (Just ctx))  =  fmap (\(c, cs, h) -> (CVar APP ns Nothing, c : cs, h)) <$> f ctx
appCtxNest' _ _                         =  return []

-------------------------------------------------------------------------------
-- Helper functions for context generation: --
-------------------------------------------------------------------------------

-- Generate contexts from the context patterns in CtxEqLib                                                                                                 
autoEqCtxs           ::  ((Ctx, [Ctx], Bool) -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) 
                         -> (CtxEqLib -> [CtxPat]) 
                         -> Ctx 
                         -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
autoEqCtxs gen f ctx  =  ask >>= (concat <$>) . sequence 
                                              . fmap gen 
                                              . catMaybes 
                                              . fmap (flip ctxPatMatchGen ctx) 
                                              . f

-- A pruned substitution into a cost-equivalent context from CtxEqLib
pruneCtxSubst                   ::  (Maybe Ctx -> Bool) -> (Ctx, [Ctx], Bool) -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
pruneCtxSubst f (ctx, [], h)     |  f Nothing    = return [(ctx, [], h)]
pruneCtxSubst f (ctx, [sub], h)  |  f (Just sub) = return [(ctx, [sub], h)]
-- Anything else means that the user has specified 
-- a context nesting pattern and /not/ a context pattern
pruneCtxSubst _ _                =  return []

-- A nesting generation from substitution into a cost-equivalent context from 
-- CtxEqLib
nestCtxSubst                 ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> (Ctx, [Ctx], Bool) -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]
nestCtxSubst f (c, [sub], h)  =  fmap (\(c', cs, _) -> (c, c' : cs, h)) <$> (f sub)
nestCtxSubst _ _              =  return []

{-
  Generate contexts from let bindings/case alternatives. This is only used when
  generating standard contexts, as eval/applicative have too specific a form 
  and value contexts don't allow let/alt statements.
-}

-- Binds
genBindsStdCtxs        ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> [Bind] -> Reader CtxEqLib [([Bind], [Ctx], Bool)]
genBindsStdCtxs gen bs  =  patSeqGen <$> sequence (fmap (genBindStdCtx gen) bs)

genBindStdCtx                         ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Bind -> Reader CtxEqLib [(Bind, [Ctx], Bool)]
genBindStdCtx gen b@(Bind ns ctx idx)  =  ifM (isStdCtx ctx) (((b, [], hasHolesBind b) :) <$> rest) rest 
                                          where rest = fmap (\(c, cs, h) -> (Bind ns c idx, cs, h)) <$> gen ctx

-- Alts
genAltsStdCtxs        ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> [Alt] -> Reader CtxEqLib [([Alt], [Ctx], Bool)]
genAltsStdCtxs gen as  =  patSeqGen <$> sequence (fmap (genAltStdCtx gen) as)

genAltStdCtx                            ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Alt -> Reader CtxEqLib [(Alt, [Ctx], Bool)]
genAltStdCtx gen a@(Alt con ns ctx idx)  =  ifM (isStdCtx ctx) (((a, [], hasHolesAlt a) :) <$> rest) rest 
                                            where rest = fmap (\(c, cs, h) -> (Alt con ns c idx, cs, h)) <$> gen ctx

{- 
  Generate let evaluation contexts of the form below:

  Given
  
     let { y = M, x_0 = A_0[x_1], x_1 = A_1[x_2], .., x_n = A_n } in A[x_0]

  we have to ensure that each binding's context A_i has been substituted 
  with a unique binder x_j from the let construct, i.e., A_i[x_j]. So we 
  effectively form a chain of substitutions. The last element of the chain
  is the context we need to generate.

  i.e., x_0 -> x_1 -> x_2 -> .. -> x_n = A_n, and then generate A_n

  The order of let bindings doesn't matter, so we have to 'sort' this list.
-}
genLetEvalCtxs                ::  (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)]) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
genLetEvalCtxs f (Let bs ctx)  =  fmap (\(bs', cs, h) -> (Let bs' ctx, cs, h)) -- Rebuild the let
                                   . catMaybes                                 -- Failed cases
                                   . validate                                  -- Validate the bs nestings against that of ctx
                                  <$> concatMapM genB genIdxs                  -- Calculate the applicative contexts over bs

                                  where
                                       -- (4) Calculate the idxs of the applicative contexts to be generated: x_n = A_n
                                       genIdxs = case ctxSubst of 
                                                  Nothing -> []
                                                  Just ns -> catMaybes $ fmap (\subChain 
                                                              -> autoAppBinderChain ns subChain) bssSubst
                                       -- (3) Calculate which binder has been subbed into the let's ctx: x_0 in A[x_0]
                                       ctxSubst = autoAppCtxBinderSubst (fmap bindBinder bs) ctx
                                       -- (2) Calculate all possible binding chains
                                       bssSubst                    = fmap (\bs -> (fmap (bSubst (fmap bindBinder bs)) bs)) part
                                       bSubst bs (Bind ns ctx idx) = (ns, autoAppCtxBinderSubst bs ctx, idx)
                                       -- (1) Divide bs into all possible subsets
                                       part = powerset bs

                                       -- Helpers
                                       holesBs  = hasHolesBinds bs 
                                       holesCtx = hasHoles ctx

                                       genB i = let Bind ns body idx = bs !! i 
                                                in catMaybes 
                                                    . fmap (\(c, cs, _) -> do 
                                                       cs' <- validateCtxNestings holesBs [cs] 
                                                       Just $ (replaceAtIdx i (Bind ns c idx) bs, cs', holesBs))
                                                   <$> f body

                                       validate = fmap (\(bs, cs, h1) -> 
                                                   let h = h1 || holesCtx 
                                                   in validateCtxNestings h [cs, []] >>= \cs' -> Just (bs, cs', h))     
genLetEvalCtxs _ _             =  return [] 

-- Helper for sequencing generations
seqGen       ::  Eq a => [(a, [Ctx], Bool)] -> [(a, [Ctx], Bool)] -> [([a], [Ctx], Bool)]
seqGen xs ys  =  patSeqGen [xs, ys]


-- Generate from context variables, as this requires a little more thought.
unrollCVarGen :: (Maybe Ctx -> Bool) -> Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)] 
unrollCVarGen f (CVar k ns Nothing)    | f Nothing    = return [(CVar k ns Nothing, [], True)]  
unrollCVarGen f (CVar k ns (Just ctx)) 
 | f (Just ctx) = ((CVar k ns Nothing, [ctx], False) :) <$> rest
 | otherwise    = rest 
    where rest = fmap (\(c, cs, h) -> (CVar k ns (Just c), cs, h)) <$> unrollCVarGen f ctx
unrollCVarGen _ _ = return []



-- Generate the function to generate a specific context nesting
genNestFun   ::  (Maybe Ctx -> Bool) -> [CtxKind] -> (Ctx -> Reader CtxEqLib [(Ctx, [Ctx], Bool)])
genNestFun f  =  gen 
                 where 
                      gen []          = const (return [])
                      gen [STD]       = autoStdCtxs  f
                      gen [VAL]       = autoValCtxs  f
                      gen [EVAL]      = autoEvalCtxs f
                      gen [APP]       = autoAppCtxs  f
                      gen (STD  : cs) = stdCtxNest   (gen cs)
                      gen (VAL  : cs) = valCtxNest   (gen cs)
                      gen (EVAL : cs) = evalCtxNest  (gen cs)
                      gen (APP  : cs) = appCtxNest   (gen cs)




-- Ensure the variables of context substitions are free

freeVarsCtxSubstsCurrScope ::  R [(Ctx, [Ctx], Bool)]
freeVarsCtxSubstsCurrScope  =  rewrite $ freeVarsCtxSubsts

freeVarsCtxSubstsNewScope ::  R [(Ctx, [Ctx], Bool)]
freeVarsCtxSubstsNewScope  =  contextfreeT $ freeVarsCtxSubsts emptyKureContext

-- Ignores let binders, will only work on lets
freeVarsCtxSubstsNoLetBindersNewScope :: R [(Ctx, [Ctx], Bool)]
freeVarsCtxSubstsNoLetBindersNewScope  
 =  rewrite $ \kc -> (catMaybes <$>) . mapM (\(c, cs, b) -> do 
     let (cs', [sub]) = splitAt (length cs - 1) cs 
     bvs <- concatMapM (applyT boundVarsHolesNoLetBindersT kc) (c : cs') 
     fvs <- freeVars sub
     if null (bvs `intersect` fvs)
        then return $ Just (c, cs, b)
        else return Nothing)

specFreeVarsCtxSubsts         ::  T Ctx [Name] 
                                  -> KureContext 
                                  -> [(Ctx, [Ctx], Bool)] 
                                  -> TM KureMEnv [(Ctx, [Ctx], Bool)]                               
specFreeVarsCtxSubsts genT kc  =  (catMaybes <$>) . mapM (\(c, cs, b) -> do 
                                   let (cs', [sub]) = splitAt (length cs - 1) cs 
                                   bvs <- applyT boundVarsHolesListT kc (c : cs') 
                                   fvs <- applyT genT emptyKureContext sub
                                   if null (bvs `intersect` fvs)
                                      then return $ Just (c, cs, b)
                                      else return Nothing)

-- Check context's free variables are not bound in the current scope.
freeVarsCtxs :: KureContext -> [(Ctx, [Ctx], Bool)] -> TM KureMEnv [(Ctx, [Ctx], Bool)] 
freeVarsCtxs kc = (catMaybes <$>) . mapM (\(c, cs, b) -> do 
                   let (cs', _) = splitAt (length cs - 1) cs 
                   fvs <- freeVarsList (c : cs') 
                   let bvs = boundVarsContext kc
                   if null (bvs `intersect` fvs)
                      then return $ Just (c, cs, b)
                      else return Nothing)


--- Context nesting checks: ---

specFreeVarsCtxNestSubsts :: T Ctx [Name] 
                          -> KureContext 
                          -> [(Ctx, [Ctx], Bool)] 
                          -> TM KureMEnv [(Ctx, Ctx, Term)]                               
specFreeVarsCtxNestSubsts genT kc = 
  (catMaybes <$>) . mapM (\(c1, c2, sub) -> do 
    bvsCtxs <- applyT boundVarsHolesListT kc [c1, c2]
    fvsSub  <- applyT genT emptyKureContext sub

    bvsC1 <- applyT boundVarsHolesT emptyKureContext c1 
    fvsC2 <- applyT freeVarsT emptyKureContext c2

    if null (bvsCtxs `intersect` fvsSub) && 
       null (bvsC1 `intersect` fvsC2)
    then return $ Just (c1, c2, sub)
    else return Nothing) . genTrips




-- Generare contexts that have free substitutions
genCtxsFreeSubstNewScope :: (Maybe Ctx -> Bool) 
                            -> [CtxKind] 
                            -> CtxEqLib 
                            -> T Ctx [(Ctx, [Ctx], Bool)] 
genCtxsFreeSubstNewScope f ks lib = 
  contextfreeT $ \ctx -> freeVarsCtxSubsts emptyKureContext 
                           (genCtxs f ks lib ctx) 






genCtxsFreeSubstCurrScope :: (Maybe Ctx -> Bool) 
                             -> [CtxKind] 
                             -> CtxEqLib 
                             -> T Ctx [(Ctx, [Ctx], Bool)] 
genCtxsFreeSubstCurrScope f ks lib =  
  transform $ \c ctx -> freeVarsCtxSubsts c (genCtxs f ks lib ctx) 


-- GOOD FUNCTIONS: --


-- Generate contexts and substitutions that have disjoint 
-- free and bound variables.
genCtxsFvBvDisjoint :: (Maybe Ctx -> Bool) 
                       -> [CtxKind] 
                       -> CtxEqLib 
                       -> T Ctx [(Ctx, [Ctx], Bool)] 
genCtxsFvBvDisjoint f ks lib = contextfreeT $ \ctx -> do 
  catMaybes <$> (mapM freeBoundDisjoint $ genCtxs f ks lib ctx)        

-- Generate contexts and substitutions that have disjoint 
-- free and bound variables, and where the substitution is 
-- free in the /current scope/.
genCtxsFvBvDisjointFreeSubstCurrScope 
 :: (Maybe Ctx -> Bool) 
    -> [CtxKind] 
    -> CtxEqLib 
    -> T Ctx [(Ctx, [Ctx], Bool)] 
genCtxsFvBvDisjointFreeSubstCurrScope f ks lib = 
  transform $ \c ctx -> freeVarsCtxSubsts c =<< catMaybes <$> 
   (mapM freeBoundDisjoint $ genCtxs f ks lib ctx) 



-- Ensure the free variables of each context are disjoint 
-- from the bound variables of the remaining ones in the nesting
-- list.
freeBoundDisjoint :: (Ctx, [Ctx], Bool) -> TM KureMEnv (Maybe (Ctx, [Ctx], Bool))
-- Last element in the list is the subst, so only the frees of the subst get
-- checked, which is what we want.
freeBoundDisjoint subst@(c, cs, _) = disj $ reverse (c : cs)
  where disj [] = return (Just subst)
        disj (subst : cs) = do 
          fvs <- applyT freeVarsT emptyKureContext subst 
          bvs <- applyT boundVarsHolesListT emptyKureContext cs
          if null (fvs `intersect` bvs)
          then disj cs
          else return Nothing


-- Ensure the free variables of a context's substitution are free at the 
-- location of the holes in each context, for a given KURE context.
freeVarsCtxSubsts :: KureContext 
                     -> [(Ctx, [Ctx], Bool)] 
                     -> TM KureMEnv [(Ctx, [Ctx], Bool)]                               
freeVarsCtxSubsts kc = (catMaybes <$>) . mapM (\(c, cs, b) -> do 
  let (cs', [sub]) = splitAt (length cs - 1) cs 
  bvs <- applyT boundVarsHolesListT kc (c : cs') 
  fvs <- freeVars sub
  if null (bvs `intersect` fvs)
     then return $ Just (c, cs, b)
     else return Nothing)
