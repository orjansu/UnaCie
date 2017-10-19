{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Subst
  ( 

    capAvoidSubstCtxR  -- Capture-avoiding substitution for contexts.
  , capAvoidSubstCtxR' -- As above, but uses an empty context.

    -- Sieving inputs to extract all variable names (free and bound): -- 

  , sieveColT
  , sieveListT
  , sieveT

    -- Generating new binders and imposing various constraints on the
    -- new names: -- 
    -- F = avoid collisions by projecting free variables from input.
  , genNewBinder
  , genNewBinderFT
  , genNewBinderFT'
  , genNewBinderT
  , genNewBinderT'
  , newName
  , safeBinders'
  , safeBindersR
  , safeBindersUR  -- Remove this
  , safeBindersUR' -- Remove this
  
    -- Safe variable substitution: --

  , substCaseR
  , substListR
  , substListVarR
  , substNonRecLetBindingsR
  , substR
  , substTryListR
  , substTryListVarR
  , substTryR
  , substTryVarR

    -- Deleting context substitutions: --

  , delCtxSubst

  ) where 

import CtxAST      (Alt(..), Bind(..), Con(..), Ctx(..), Name)
import KureMonad   (TM)
import Crumb       (Crumb)
import KureContext (emptyKureContext)
import Universes   (U(..))
import Classes     (AddBinders(..), SafeNames(..), freshVar)
import CtxUtils    (bindsToPairs, hasHoles, isDatatype, substCtx)
import Utils       ((.*), eitherHead)
import KureExtra   (any0tdR, applyAtSnocPathR, concatMapT, liftT)
import TransUtils  ( absBinderT, altBindersT, bindBinderT
                   , boundVarsHolesT, caseBindersT, freeVars, freeVarsT
                   , freshVarFT, freshVarT, letBindersT
                   , specCtxPathsT, varNameT )
import Kure        ( absAllR, absTManBind, altAllR, altTManBind
                   , appAnyR, appDAnyR', bindAllR, cVarAllR
                   , caseAllR, caseAnyR, caseT, letAllR
                   , letTManBind, tickAllR, varT)

import Control.Arrow  ((>>>))
import Data.Maybe     (catMaybes)
import Data.List      ((\\), intersect)
import Control.Monad.State (MonadState)
import Language.KURE.ExtendableContext (ExtendContext)

import Language.KURE
  ( ExtendPath
  , Injection
  , LocalPath
  , MonadCatch
  , ReadPath
  , Rewrite
  , Transform
  , Walker
  , (<+)
  , allT
  , applyR
  , applyT
  , catchesM
  , changedR
  , constT
  , contextonlyT
  , crushtdT
  , extractR
  , extractT
  , guardM
  , guardMsgM
  , idR
  , inject
  , liftContext
  , promoteR
  , promoteT
  , repeatR
  , setFailMsg
  , transform
  , tryR
  , whenM
  )

{-
  <TO-DO>: - Improve substCaseR.
           - Move context substitution functions from CtxUtils here? 
           - Remove export of safeBindersUR'

  Information:
  -----------------------------------------------------------------------------
  - Sieving to extract *all* variables (both free and bound);
  - Safe variable substitution;
  - Some functions for /deleting/ context substitution(s).
-}

-------------------------------------------------------------------------------
-- Sieving contexts: --
-------------------------------------------------------------------------------
{-
  - Sieving contexts gets all the variable names/binders and returns them in a
    list;
  - It comes in handy for things like renaming binders to avoid capture. 
    
    (It annoys me when the renaming of a binder to avoid variable capture 
    results in other binders having to be renamed for the same reason. This 
    can be prevented by sieving and then ensuring any new name chosen doesn't 
    appear in the resulting list of names.)
-}

-- Get all variable names/binders: --------------------------------------------

sieveT :: ( ExtendPath c Crumb, ReadPath c Crumb
          , MonadCatch m, AddBinders c, Walker c U
          , Injection a U ) 
          => Transform c m a [Name]
sieveT  = extractT sieveUT

sieveUT :: ( ExtendPath c Crumb, ReadPath c Crumb
           , MonadCatch m, AddBinders c, Walker c U ) 
           => Transform c m U [Name]
sieveUT  = idR >>= \case
            UGBind{} -> allT ctxT
            _        -> ctxT
           where ctxT = crushtdT   -- All variables (free or bound).
                                 $ promoteT (return <$> varNameT) 
                                   -- All abs. binders.
                                <+ promoteT (return <$> absBinderT) 
                                   -- All let binders.
                                <+ promoteT (return <$> bindBinderT) 
                                   -- All alt binders.
                                <+ promoteT altBindersT

-- Sieving /lists/ of input: --

sieveListT :: (ExtendPath c Crumb, ReadPath c Crumb
              , MonadCatch m, AddBinders c, Walker c U
              , Injection a U ) 
              => Transform c m [a] [Name]
sieveListT  = concatMapT sieveT

-- sieveListUT :: (ExtendPath c Crumb, ReadPath c Crumb
--                , MonadCatch m, AddBinders c, Walker c U ) 
--                => Transform c m [U] [Name]
-- sieveListUT  = concatMapT sieveUT

-- Get variable names/binders that could cause /immediate/ capture/collision: - 
-- Note: this does not get bound variables in binding context's bodies, so 
-- using this /can/ result in knock-on renames.

sieveColT :: ( MonadCatch m, AddBinders c
             , Walker c U, ExtendPath c Crumb
             , ReadPath c Crumb, Injection a U ) 
             => Transform c m a [Name]
sieveColT  = extractT sieveColUT

sieveColUT :: ( MonadCatch m, AddBinders c, Walker c U, ExtendPath c Crumb
              , ReadPath c Crumb ) => Transform c m U [Name]
sieveColUT  = idR >>= \case
               UGBind{} -> allT freeVarsT
               UCtx{}   -> abs <+ tel <+ esac <+ freeVarsT
               UBind{}  -> (:)  <$> promoteT bindBinderT <*> freeVarsT
               UAlt{}   -> (++) <$> promoteT altBindersT <*> freeVarsT
              where 
                abs  = (:)  <$> promoteT absBinderT   <*> freeVarsT
                tel  = (++) <$> promoteT letBindersT  <*> freeVarsT
                esac = (++) <$> promoteT caseBindersT <*> freeVarsT

-------------------------------------------------------------------------------
-- Generate new binders for renaming: --
-------------------------------------------------------------------------------
{-
  - Our approach to binder generation is to either take a hint from an old but 
    invalid binder, and modify it by adding an integer suffix such that it 
    becomes valid, or, if no hint is given, we take one from the infinite
    list of variables in the KURE context;
  - We take a list of invalid names to ensure no capture/collisions;
  - Some functions also sieve inputs to ensure no capture/collisions.
-}

-- No invalid name list parameter
genNewBinderT' :: ( ExtendPath c Crumb, ReadPath c Crumb
                  , AddBinders c, SafeNames s, Walker c U
                  , Injection a U, MonadState s m, MonadCatch m )
                  => Maybe Name -> Transform c m a Name  
genNewBinderT'  = genNewBinderT []
    
-- Sieve the input to make sure no variable capture/binder collisions                                                     
genNewBinderT :: ( ExtendPath c Crumb, ReadPath c Crumb
                 , AddBinders c, SafeNames s, Walker c U
                 , Injection a U, MonadState s m, MonadCatch m )
                 => [Name] -> Maybe Name -> Transform c m a Name 
genNewBinderT invalid mns = 
  (invalid ++) <$> sieveT >>= \invalid' -> case mns of 
    Just ns -> return (newName ns invalid')
    Nothing -> freshVarT invalid'
 
-- As above but we don't sieve, so we assume all invalid names are in the 
-- 'invalid' parameter.                        
genNewBinder :: (MonadState s m, SafeNames s) => [Name] -> Maybe Name -> m Name  
genNewBinder invalid Nothing   = freshVar invalid 
genNewBinder invalid (Just ns) = return (newName ns invalid)

-- We avoid collisions only by projecting free variables from the argument.
genNewBinderFT :: ( AddBinders c, SafeNames s, Walker c U, Injection a U
                  , MonadState s m, MonadCatch m, ReadPath c Crumb
                  , ExtendPath c Crumb )
                  => [Name] -> Maybe Name -> Transform c m a Name 
genNewBinderFT invalid Nothing   = freshVarFT invalid
genNewBinderFT invalid (Just ns) = newName ns . (invalid ++) <$> freeVarsT
  
-- As above but empty parameter.
genNewBinderFT' :: ( AddBinders c, SafeNames s, Walker c U, Injection a U
                   , MonadState s m, MonadCatch m, ReadPath c Crumb
                   , ExtendPath c Crumb )
                   => Maybe Name -> Transform c m a Name   
genNewBinderFT'  = genNewBinderFT [] 

-- Performs the new name generation by adding integer suffixes
-- E.g., x invalid then we try x0, x1, x2 etc.  
newName :: Name -> [Name] -> Name 
newName ns = head . ((\\) (ns : fmap (\i -> ns ++ show i) [0 :: Int ..]))

-------------------------------------------------------------------------------
-- Modifying binders: --
-------------------------------------------------------------------------------
{- 
  - Make binders 'safe' with respect to a given list of free variables:
  -- I.e., alpha-rename any binders that would cause variable capture;
  -- We sieve to prevent 'knock-on' renames for consistency
     (A knock-on rename is when the renaming of one binder -- to prevent 
     capture -- causes the renaming of another further down the line for the 
     same reason. We want to avoid this because primarily I don't like it).

  - Note the above safety guarantee is only with respect to any immediate 
    binders, it is /not/ recursive. 
  -- This means once this function has been applied, we can safely substitute
     inside the bodies' of the binding contexts, but we can't /recursively/ 
     substitute without further safety checks;
  -- For our substitution function (substR), the safety checks would always 
     be performed anyway, but any others must also do the same.
-}

safeBinders' :: (Injection a U, SafeNames s) => [Name] -> a -> TM s a
safeBinders' fvs = applyR (extractR $ safeBindersUR' fvs) 
                    emptyKureContext . inject

safeBindersR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                , SafeNames s, Injection a U, MonadState s m, MonadCatch m ) 
                => [Name] -> Rewrite c m a
safeBindersR  = extractR . safeBindersUR'

-- As below but no invalid names given
safeBindersUR' :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                  , SafeNames s, MonadState s m, MonadCatch m ) 
                  => [Name] -> Rewrite c m U
safeBindersUR'  = flip safeBindersUR []

-- We take a list of invalid names just in case we want to restrict the new
-- name in some way;
-- Here we manually add bindings to KURE's context using ManBind congruence
-- combinators. This is a special case because we are in fact manipulating 
-- binders.
safeBindersUR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                 , SafeNames s, MonadState s m, MonadCatch m ) 
                 => [Name] -> [Name] -> Rewrite c m U
safeBindersUR fss invalid = idR >>= \case
  UGBind{} -> idR
  UCtx{}   -> promoteR $ abs <+ tel <+ caseAllR idR (const alt) <+ idR
  UBind{}  -> fail "cannot make let bindings safe in isolation because \
               \they are mutually recursive."
  UAlt{}   -> promoteR alt
  where 
  abs = do 
          bs   <- absBinderT
          used <- sieveT 
          ([bs'], subs) <- constT $ freshNames [bs] fss (invalid ++ used ++ fss)
          absTManBind bs' idR (substTryListVarR subs) (const $ Abs bs')
  tel = do 
          bss  <- letBindersT
          used <- sieveT
          (bss', subs) <- constT $ freshNames bss fss (invalid ++ used ++ fss)
          let s = substTryListVarR subs
          letTManBind bss' (\i -> bindAllR (return $ bss' !! i) s idR) s Let
  alt = do 
          bss  <- altBindersT
          used <- sieveT 
          (bss', subs) <- constT $ freshNames bss fss (invalid ++ used ++ fss)
          altTManBind bss' idR (\i -> return $ bss' !! i) 
           (substTryListVarR subs) idR Alt

  freshNames bss fss used = (fmap catMaybes . unzip <$>) . mapM newName $ bss
   where newName bs 
          | bs `elem` fss = do 
                              bs' <- genNewBinder (used ++ fss) (Just bs)
                              return (bs', Just (bs, bs')) 
          | otherwise     = return (bs, Nothing)

-------------------------------------------------------------------------------
-- Variable substitution: --
-------------------------------------------------------------------------------
{- 
  - Safe variable substitution;
  - Perform alpha-renaming as little as possible to aid the readability/
    consistency of proofs;
  - To achieve this, the substitution is checked for success /prior/ to 
    renaming;
  - Renaming also actively avoids knock-on renaming.
-}
substR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
          , SafeNames s, MonadState s m, MonadCatch m ) 
          => Name -> Ctx -> Rewrite c m Ctx
substR ns ctx = 
  setFailMsg "substR failed: nothing to substitute." $ 
  catchesM [var, abs, app, tick, tel, esac, appD, cVar]
  where
    var  = whenM (varT (== ns)) (return ctx)
    app  = appAnyR (substR ns ctx) (substR ns ctx)
    tick = tickAllR (substR ns ctx)
    appD = appDAnyR' (const $ substR ns ctx) -- appDAnyR body only
    cVar = cVarAllR idR idR (substR ns ctx) 

    abs = do
      bs   <- absBinderT
      guardM (bs /= ns) -- Check for shadowing.
      fss1 <- liftContext (nullBinders) freeVarsT
      guardM (ns `elem` fss1) -- Here we check the subst. will succeed.
      fss2 <- contextonlyT $ \c -> applyT freeVarsT (nullBinders c) ctx
      extractR (safeBindersUR' fss2) >>> absAllR idR (substR ns ctx)
               
    tel = do 
      bss <- letBindersT
      guardM (ns `notElem` bss)
      fss1 <- liftContext (nullBinders) freeVarsT
      guardM (ns `elem` fss1)
      fss2 <- contextonlyT $ \c -> applyT freeVarsT (nullBinders c) ctx
      let s = substTryR ns ctx
      extractR (safeBindersUR' fss2) >>> letAllR (const $ bindAllR idR s idR) s

    esac = caseAnyR (substR ns ctx) . const $ do 
      bss <- altBindersT
      guardM (ns `notElem` bss)
      fss1 <- liftContext (nullBinders) freeVarsT
      guardM (ns `elem` fss1)
      fss2 <- contextonlyT $ \c -> applyT freeVarsT (nullBinders c) ctx
      extractR (safeBindersUR' fss2) >>> altAllR idR (const idR) (substTryR ns ctx) idR
 
-- Substitution variants: -----------------------------------------------------
{- 
  Note: converting failing substitutions to identities is important when 
  renaming binders, as we can't necessarily assume applied occurrences of a 
  binder exist, even though the binder itself does.

  Simple example: rename the binder y in \y.x. 
-}

-- substR that converts failures to identities.
substTryR :: ( ExtendPath c Crumb, ReadPath c Crumb
             , AddBinders c, SafeNames s, MonadState s m, MonadCatch m ) 
             => Name -> Ctx -> Rewrite c m Ctx
substTryR  = tryR .* substR

-- Apply a list of substitutions, will only succeed if
-- all individual substitutions succeed.
substListR                  :: ( ExtendPath c Crumb, ReadPath c Crumb
                               , AddBinders c, SafeNames s, MonadState s m
                               , MonadCatch m ) 
                               => [(Name, Ctx)] -> Rewrite c m Ctx
substListR []                = idR 
substListR ((ns, sub) : nss) = substR ns sub >>> substListR nss

-- As above, but converts failures to identities.
substTryListR                  :: ( ExtendPath c Crumb, ReadPath c Crumb
                                  , AddBinders c, SafeNames s, MonadState s m
                                  , MonadCatch m ) 
                                  => [(Name, Ctx)] -> Rewrite c m Ctx
substTryListR []                = idR 
substTryListR ((ns, sub) : nss) = tryR (substR ns sub) >>> substTryListR nss

-- substR/substTryR on variables only: --

substVarR    :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                , SafeNames s, MonadState s m, MonadCatch m ) 
                => Name -> Name -> Rewrite c m Ctx
substVarR old = substR old . Var

substTryVarR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                , SafeNames s, MonadState s m, MonadCatch m ) 
                => Name -> Name -> Rewrite c m Ctx
substTryVarR  = tryR .* substVarR 

-- substListR/substTryListR on variables only: --

substListVarR ::  ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                  , SafeNames s, MonadState s m, MonadCatch m ) 
                  => [(Name, Name)] -> Rewrite c m Ctx
substListVarR  =  substListR . fmap (fmap Var)

substTryListVarR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                    , SafeNames s, MonadState s m, MonadCatch m ) 
                    => [(Name, Name)] -> Rewrite c m Ctx
substTryListVarR  = substTryListR . fmap (fmap Var)

-- Substitute from let bindings: ---------------------------------------------- 

-- Substitute all non-recursive let bindings. Substs. are exhausted until a 
-- fixed point is reached. Warning: if used with recursive let bindings, 
-- this will /not/ terminate;
-- We use this for syntactic sugar.
substNonRecLetBindingsR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                           , SafeNames s, MonadState s m, MonadCatch m ) 
                           => [Bind] -> Rewrite c m Ctx
substNonRecLetBindingsR  = repeatR . changedR . substTryListR . bindsToPairs

-- Case alternatives substitution: --------------------------------------------

-- This is just an ad-hoc implementation, it works fine but need to something 
-- more precise?
substCaseR :: forall c m s . 
              ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
              , MonadCatch m, MonadState s m, SafeNames s ) 
              => Rewrite c m Ctx
substCaseR = idR >>= \(Case scrut _) -> if isDatatype scrut 
  then caseT idR (const $ matchAlt scrut) (\_ x -> firstMatch x) >>= either fail return
  else fail "case scrutinee not a datatype."
  where 
    firstMatch :: [Maybe Ctx] -> Either String Ctx
    firstMatch  = eitherHead "case not exhaustive." . catMaybes

    matchAlt :: Ctx -> Transform c m Alt (Maybe Ctx)
    matchAlt (LitInt i) = transform $ \c alt -> case alt of
     (Alt (LITINT i') _    ctx _) | i == i' -> return (Just ctx)
     (Alt VARIABLE    [ns] ctx _) -> Just <$> applyR (substTryR ns $ LitInt i) c ctx
     (Alt DEFAULT     _    ctx _) -> return (Just ctx)
     _ -> return Nothing
    matchAlt (LitStr s) = transform $ \c alt -> case alt of
     (Alt (LITSTR s') _    ctx _) | s == s' -> return (Just ctx)
     (Alt VARIABLE    [ns] ctx _) -> Just <$> applyR (substTryR ns $ LitStr s) c ctx
     (Alt DEFAULT     _    ctx _) -> return (Just ctx)
     _ -> return Nothing                           
    matchAlt ctx' = transform $ \c alt -> case alt of
     (Alt VARIABLE    [ns] ctx _) -> Just <$> applyT (substTryR ns ctx') c ctx
     (Alt DEFAULT     _    ctx _) -> return (Just ctx)
     (Alt con         nss  ctx _) -> maybe (return Nothing) 
                                     (\sl -> Just <$> applyR (substTryListR sl) c ctx)
                                     (zipArgs con nss ctx')

    zipArgs :: Con -> [Name] -> Ctx -> Maybe [(Name, Ctx)]
    zipArgs con nss (AppD con' ctxs) 
     | con == con' && length nss == length ctxs = Just (zip nss ctxs) 
     | otherwise                                = Nothing 
    zipArgs _ _ _                               = Nothing 

-------------------------------------------------------------------------------
-- Context substitution: --
-------------------------------------------------------------------------------

-- Capture avoiding substitution for contexts.
-- Note: this is an alternative to substCtx (substCtx assumes variable capture
-- is the intended result).
capAvoidSubstCtxR :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                     , MonadCatch m, MonadState s m, SafeNames s
                     , Walker c U ) 
                     => Ctx 
                     -> Rewrite c m Ctx
capAvoidSubstCtxR sub = do 
  guardMsgM (liftT hasHoles) "no holes to substitute."
  fvs <- freeVars sub 
  bvs <- boundVarsHolesT 
  (extractR . any0tdR . safeBindersUR' $ fvs `intersect` bvs) >>> liftT (substCtx sub)


-- As above, but uses an empty context.
capAvoidSubstCtxR' :: ( ExtendPath c Crumb, ReadPath c Crumb, AddBinders c
                      , MonadCatch m, MonadState s m, SafeNames s
                      , Walker c U ) 
                      => Ctx 
                      -> Rewrite c m Ctx
capAvoidSubstCtxR' sub = liftContext nullBinders $ do 
  guardMsgM (liftT hasHoles) "no holes to substitute."
  fvs <- freeVars sub 
  bvs <- boundVarsHolesT 
  (extractR . any0tdR . safeBindersUR' $ fvs `intersect` bvs) >>> liftT (substCtx sub)

-- Replace the substitution(s) of a context with holes;
-- We just find all paths to the subst. and then return a hole at that 
-- location.
delCtxSubst :: ( MonadCatch m, ExtendPath c Crumb
               , ReadPath c Crumb, AddBinders c
               , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
               , AddBinders (ExtendContext c (LocalPath Crumb)) ) 
               => Ctx -> Rewrite c m Ctx
delCtxSubst sub = 
  extractT (specCtxPathsT sub) >>= \case 
    []       -> fail "invalid substitution."
    subPaths -> extractR $ f subPaths
  where 
    f []       = idR 
    f (p : ps) = applyAtSnocPathR p (return $ UCtx Hole) >>> f ps