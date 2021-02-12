{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module TransUtils
  (
    -- Injections/projections: --

    extractRU
  , extractTU
  , injectU
  , projectU
  , promoteRU
  , promoteTU

  -- Free/bound variables: --

  , boundVars
  , boundVarsContextT
  , boundVarsHoles
  , boundVarsHolesListT
  , boundVarsHolesNoLetBindersT
  , boundVarsHolesT
  , boundVarsList
  , boundVarsListT
  , boundVarsT
  , freeVars
  , freeVarsList
  , freeVarsListT
  , freeVarsT

  -- Fresh variable generation: --
  -- B for avoiding bounds in KureContext;
  -- F for avoiding frees in parameter.

  , freshVarBFT
  , freshVarBFT'
  , freshVarBT
  , freshVarBT'
  , freshVarFT
  , freshVarFT'
  , freshVarT
  , freshVarT'

  -- Basic transformations: --

  , absBinderT
  , absBodyR
  , altBindersT
  , appDBodyT
  , bindBinderT
  , caseAltCtxsT
  , caseBindersT
  , caseBindersT'
  , caseScrutR
  , deleteLetBindingR
  , deleteUnusedLetBindingR
  , deleteUnusedLetBindingsR
  , letBindersT
  , letBindingCtxsT
  , letBindingLookupT
  , letBindingsT
  , letBodyR
  , substEmptyLetBindingsR
  , tickBodyR
  , varNameT

 -- Checking syntactic form: --

  , absAccepterT
  , appAccepterT
  , appDAccepterT
  , cVarAccepterT
  , caseAccepterT
  , funAppAccepterT
  , holeAccepterT
  , letAccepterT
  , redexAccepterT
  , tickAccepterT
  , valAccepterT
  , varAccepterT

 -- Path generation: --

  , holePathsT
  , specCVarPathsT
  , specCtxPathT
  , specCtxPathsT
  , specFreeVarPathsT
  , specVarPathT
  , specVarPathsT

  -- Pretty printing: --

  , altToShowAltT
  , bindToShowBindT
  , ctxToShowCtxT
  , gBindToShowGBindT
  , uToShowUT

  ) where

import Classes     ( AddBinders(..), SafeNames(..), boundVarsContext
                   , freshVar, isBoundContext, isFreeContext )
import Crumb       (Crumb(..))
import CtxAST      ( Alt(..), Bind(..), Ctx(..), GBind(..)
                   , L_Alt(..), L_Bind(..), L_Ctx(..)
                   , L_GBind(..), Name  )
import CtxKind     (CtxKind)
import CtxShowAST  (ShowSettings)
import CtxUtils    (altNames, bindBinder, isSpecCVar, isSpecVar)
import KureContext (emptyKureContext)
import KureExtra   (concatMapT, liftT)
import Universes   (L_U(..), U(..))
import Utils       ((.*), concatMapM, groupOthers)
import CtxUtils    ( isAbs, isApp, isCVar, isCase, isDatatype, isFunApp
                   , isHole, isLet, isNonSubstCVar, isRedex
                   , isTick, isVal, isVar, lookupBind
                   , reindexBinds )
import Kure        ( absT, altCtxT, altT, appDT, appT, bindCtxT
                   , bindT, cBindT, cVarT, caseT, letT
                   , letTNoBind, tBindT, tickT, varT )

import Language.KURE.Pathfinder        (onePathToT, pathsToT)
import Language.KURE.ExtendableContext (ExtendContext)
import Control.Monad                   (void)
import Data.List                       (union)
import Control.Arrow                   ((>>>))
import Control.Monad.State             (MonadState)

import Language.KURE
  ( AbsolutePath
  , ExtendPath
  , Injection
  , LocalPath
  , MonadCatch
  , ReadPath
  , Rewrite
  , Transform
  , Walker
  , (@@)
  , absPath
  , acceptWithFailMsgR
  , applyT
  , constT
  , contextT
  , contextfreeT
  , crushbuT
  , exposeT
  , extractR
  , extractT
  , guardM
  , guardMsgM
  , idR
  , inject
  , mapT
  , prefixFailMsg
  , project
  , promoteR
  , promoteT
  , repeatR
  , transform
  )

{-
  <TO-DO>: - Add more comments to 'Show' functions at bottom of file.

  Information:
  -----------------------------------------------------------------------------
  - A variety of helper functions for transformations/rewrites.
-}

-- Injections/projections into/out of U: --------------------------------------
-- If helper functions defined in where clauses don't have types, these give
-- GHCI the necessary type information to avoid ambiguity errors.

injectU :: Injection a U => a -> U
injectU  = inject

projectU :: Injection  a U => U -> Maybe a
projectU  = project

promoteRU :: (Monad m, Injection a U) => Rewrite c m a -> Rewrite c m U
promoteRU  = promoteR

promoteTU :: (Monad m, Injection a U) => Transform c m a b -> Transform c m U b
promoteTU  = promoteT

extractRU :: (Monad m, Injection a U) => Rewrite c m U -> Rewrite c m a
extractRU  = extractR

extractTU :: (Monad m, Injection a U) => Transform c m U b -> Transform c m a b
extractTU  = extractT

-- Free variables:  -----------------------------------------------------------

-- Calculate the free variables of a given input.
freeVarsT :: ( MonadCatch m, AddBinders c
             , Walker c U, Injection a U )
             => Transform c m a [Name]
freeVarsT  = prefixFailMsg "freeVarsT failed: " $ extractT freeVarsUT

-- As above but for U.
freeVarsUT :: (MonadCatch m, AddBinders c, Walker c U)
              => Transform c m U [Name]
freeVarsUT  = prefixFailMsg "freeVarsUT failed: " $
              crushbuT $ do
               (c, UCtx (Var ns)) <- exposeT
               [ns] <$ guardM (ns `isFreeContext` c)

-- Calculate in an empty context
freeVars :: (MonadCatch m, Injection a U) => a -> m [Name]
freeVars  = prefixFailMsg "freeVars failed: "
             . applyT freeVarsUT emptyKureContext
             . inject

-- Bound variables: -----------------------------------------------------------

-- Project out the bound variables stored in KURE's context.
boundVarsContextT :: (Monad m, AddBinders c) => Transform c m a [Name]
boundVarsContextT  = boundVarsContext <$> contextT

-- Calculate the bound variables of a given input.
boundVarsT :: ( MonadCatch m, AddBinders c, Walker c U
              , Injection a U )
              => Transform c m a [Name]
boundVarsT  = prefixFailMsg "boundVarsT failed: " $
               extractT boundVarsUT

-- As above but for U.
boundVarsUT :: (MonadCatch m, AddBinders c, Walker c U)
               => Transform c m U [Name]
boundVarsUT  = prefixFailMsg "boundVarsUT failed: " $
               crushbuT $ do
                (c, UCtx (Var ns)) <- exposeT
                [ns] <$ guardM (ns `isBoundContext` c)

-- Calculate in an empty context.
boundVars :: (MonadCatch m, Injection a U) => a -> m [Name]
boundVars  = prefixFailMsg "boundVars failed: "
              . applyT boundVarsUT emptyKureContext
              . inject

-- Bound variables at the location of holes: ----------------------------------
-- Needed when checking if context substitutions are valid.

-- Calculate the variables of a given input that are bound
-- at the location of /holes/.
boundVarsHolesT :: ( MonadCatch m, AddBinders c, ReadPath c Crumb
                   , ExtendPath c Crumb, Injection a U )
                   => Transform c m a [Name]
boundVarsHolesT  = prefixFailMsg "boundVarsHolesT failed: " $
                    extractT boundVarsHolesUT

-- As above but for U.
boundVarsHolesUT :: (MonadCatch m, AddBinders c, Walker c U)
                    => Transform c m U [Name]
boundVarsHolesUT  = prefixFailMsg "boundVarsHolesUT failed: " $
                    crushbuT $ do
                     (c, UCtx Hole) <- exposeT
                     return (boundVarsContext c)

-- Calculate in an empty context.
boundVarsHoles :: (MonadCatch m, Injection a U) => a -> m [Name]
boundVarsHoles  = prefixFailMsg "boundVarsHoles failed:"
                   . applyT boundVarsHolesUT emptyKureContext
                   . inject

-- Similar to boundVarsHolesT but to be used on let statements only when we
-- want to ignore their binders;
-- This is used e.g., when substituting contexts /inside/ let bindings.
boundVarsHolesNoLetBindersT :: ( MonadCatch m, AddBinders c, ReadPath c Crumb
                               , ExtendPath c Crumb )
                               => Transform c m Ctx [Name]
boundVarsHolesNoLetBindersT
 = prefixFailMsg "boundVarsHolesNoLetBindersT failed: " $
   letTNoBind (const $ bindCtxT boundVarsHolesT)
    boundVarsHolesT ((++) . concat)

-- Free/bound variables from a list of inputs: --------------------------------
-- Make use of concatMapT in KureExtra.hs, useful for e.g., let bindings.

freeVarsListT :: (MonadCatch m, AddBinders c, Walker c U, Injection a U)
                 => Transform c m [a] [Name]
freeVarsListT  = prefixFailMsg "freeVarsListT failed: " $ concatMapT freeVarsT

-- freeVarsListUT :: (MonadCatch m, AddBinders c, Walker c U)
--                   => Transform c m [U] [Name]
-- freeVarsListUT  = prefixFailMsg "freeVarsListUT failed: " $
--                    concatMapT freeVarsUT

freeVarsList :: (MonadCatch m, Injection a U) => [a] -> m [Name]
freeVarsList  = prefixFailMsg "freeVarsList failed: " . concatMapM freeVars

boundVarsListT :: (MonadCatch m, AddBinders c, Walker c U , Injection a U)
                  => Transform c m [a] [Name]
boundVarsListT  = prefixFailMsg "boundVarsListT failed: " $ concatMapT boundVarsT

-- boundVarsListUT :: (MonadCatch m, AddBinders c, Walker c U)
--                    => Transform c m [U] [Name]
-- boundVarsListUT  = prefixFailMsg "boundVarsListUT failed: " $
--                     concatMapT boundVarsUT

boundVarsList :: (MonadCatch m, Injection a U) => [a] -> m [Name]
boundVarsList  = prefixFailMsg "boundVarsList failed: " . concatMapM boundVars

boundVarsHolesListT :: ( MonadCatch m, AddBinders c, ReadPath c Crumb
                       , ExtendPath c Crumb, Injection a U )
                       => Transform c m [a] [Name]
boundVarsHolesListT  = prefixFailMsg "boundVarsHolesListT failed: " $
                        concatMapT boundVarsHolesT

-- Generating fresh variables: ------------------------------------------------
{-
  freshVar :: (MonadState s m, SafeNames s) => [Name] -> m Name

  - Basic way of accessing fresh variable names;
  - Need to provide a list of invalid ones to prevent capture;
  - Functions below extend this to extract invalid names from arguments/KURE's
    context.
-}

-- freshVar lifted to Transform: --

freshVarT :: (MonadState s m, SafeNames s) => [Name] -> Transform c m a Name
freshVarT  = constT . freshVar

freshVarT' :: (MonadState s m, SafeNames s) => Transform c m [Name] Name
freshVarT'  = contextfreeT freshVar

-- Various extensions of freshVarT that take into consideration free variables
-- of the given input and bound variables in KURE's context.

-- Takes a list of invalid names to prevent capture;
-- Gets free variables from the transformation parameter to prevent capture.
freshVarFT    :: ( Injection a U, MonadCatch m, MonadState s m, SafeNames s
                 , ReadPath c Crumb, ExtendPath c Crumb, AddBinders c )
                 => [Name]
                 -> Transform c m a Name
freshVarFT nss = prefixFailMsg "freshVarFT failed: " $
                 transform $ \c x -> do
                  fvs <- applyT freeVarsT (nullBinders c) x
                  freshVar (nss ++ fvs)

-- As above but empty parameter.
freshVarFT' :: ( Injection a U, MonadCatch m, MonadState s m, SafeNames s
               , ReadPath c Crumb, ExtendPath c Crumb, AddBinders c )
               => Transform c m a Name
freshVarFT'  = prefixFailMsg "freshVarFT' failed: " $ freshVarFT []

-- Takes a list of invalid names to prevent capture;
-- Gets bound variables from the context to avoid re-use (for readability).
freshVarBT    :: (Injection a U, SafeNames s, AddBinders c, MonadState s m)
                 => [Name]
                 -> Transform c m a Name
freshVarBT nss = ((nss ++) <$> boundVarsContextT) >>> freshVarT'

-- As above but empty parameter.
freshVarBT' :: (Injection a U, SafeNames s, AddBinders c, MonadState s m)
               => Transform c m a Name
freshVarBT'  = freshVarBT []

-- Takes a list of invalid names to prevent capture;
-- Gets free variables from the transformation parameter to prevent capture;
-- Gets bound variables from the context to avoid re-use (for readability).
freshVarBFT    :: ( Injection a U, MonadCatch m, MonadState s m, SafeNames s
                  , ReadPath c Crumb, ExtendPath c Crumb, AddBinders c )
                  => [Name]
                  -> Transform c m a Name
freshVarBFT nss = prefixFailMsg "freshVarBFT failed: " $
                  transform $ \c x -> do
                   fs <- applyT freeVarsT (nullBinders c) x
                   freshVar (nss ++ boundVarsContext c ++ fs)

-- As above but empty parameter.
freshVarBFT' :: ( Injection a U, MonadCatch m, MonadState s m, SafeNames s
                , ReadPath c Crumb, ExtendPath c Crumb, AddBinders c )
                => Transform c m a Name
freshVarBFT'  = prefixFailMsg "freshVarBFT' failed: " $ freshVarBFT []

-- Common transformations/rewrites: -------------------------------------------

-- Var: --

-- Variable name.
varNameT :: Monad m => Transform c m Ctx Name
varNameT  = varT id

-- Abs: --

-- Bound variable of an abstraction.
absBinderT :: ( ExtendPath c Crumb, ReadPath c Crumb
              , AddBinders c, Monad m )
              => Transform c m Ctx Name
absBinderT  = absT idR idR const

-- Body of an abstraction.
absBodyR :: ( ExtendPath c Crumb, ReadPath c Crumb
            , AddBinders c, Monad m )
            => Rewrite c m Ctx
absBodyR  = absT idR idR (flip const)

-- Let: --

-- Bound variables of a let statement.
letBindersT :: ( ExtendPath c Crumb, ReadPath c Crumb
               , AddBinders c, Monad m )
               => Transform c m Ctx [Name]
letBindersT  = letT (const $ bindBinderT) idR const

-- Bindings of a let statement.
letBindingsT :: ( ExtendPath c Crumb, ReadPath c Crumb
                , AddBinders c, Monad m )
                => Transform c m Ctx [Bind]
letBindingsT  = letT (const idR) idR const

-- Body of a let statement.
letBodyR :: ( ExtendPath c Crumb, ReadPath c Crumb
            , AddBinders c, Monad m )
            => Rewrite c m Ctx
letBodyR  = letT (const idR) idR (flip const)

-- Ctxs of bindings of a let statement.
letBindingCtxsT :: ( ExtendPath c Crumb, ReadPath c Crumb
                   , AddBinders c, Monad m ) => Transform c m Ctx [Ctx]
letBindingCtxsT  = letBindingsT >>> mapT (bindCtxT idR)

-- Specific binder of a let statement.
letBindingLookupT :: Monad m => Name -> Transform c m Ctx (Maybe Bind)
letBindingLookupT  = liftT . lookupBind

-- Substitute (temporary) empty let bindings with actual bindings.
substEmptyLetBindingsR :: Monad m => [Bind] -> Rewrite c m Ctx
substEmptyLetBindingsR bs = idR >>= \case
  Let [] body -> return (Let bs body)
  _           -> fail "not a let with empty bindings."

-- Delete a let binding.
deleteLetBindingR :: MonadCatch m => Name -> Rewrite c m Ctx
deleteLetBindingR ns
 = prefixFailMsg "deleteLetBindingR failed: " $
   letAccepterT >> letBindingLookupT ns >>= \case
    Nothing             -> fail "invalid binder."
    Just (Bind _ _ idx) -> do
     Let bs body <- idR
     let (l, _ : r) = splitAt idx bs
     let bs' = l ++ r
     if null bs'
        then return body
        else return $ Let (reindexBinds bs') body

-- Delete /all/ let bindings that are unused.
deleteUnusedLetBindingsR :: ( AddBinders c, ExtendPath c Crumb
                            , ReadPath c Crumb, MonadCatch m )
                            => Rewrite c m Ctx
deleteUnusedLetBindingsR
 = prefixFailMsg "deleteUnusedLetBindingsR failed: " $ do
    letAccepterT
    Let bs body <- (repeatR $ do
       Let bs body <- idR
       fvss <- fmap concat . groupOthers <$> letTNoBind (const $ bindCtxT
        freeVarsT) freeVarsT (\fvss fvs -> fmap (fvs ++) fvss ++ [fvs])
       let new_bs = fmap fst $ filter f (zip bs fvss)
       if bs == new_bs
          then fail "no unused bindings."
          else return (Let new_bs body))
    if null bs
       then return body
       else return (Let bs body)


   where f (Bind bs _ _, fvs) = bs `elem` fvs

-- Delete a single unused let bindings (with safety check).
deleteUnusedLetBindingR :: ( AddBinders c, ExtendPath c Crumb
                           , ReadPath c Crumb, MonadCatch m )
                           => Name -> Rewrite c m Ctx
deleteUnusedLetBindingR ns
 = prefixFailMsg "deleteUnusedLetBindingR failed: " $ do
     guardMsgM ((ns `notElem`) <$> letTNoBind (const $ bindCtxT freeVarsT)
      freeVarsT ((++) . concat)) "binding in use."
     deleteLetBindingR ns

-- Case: --

-- Bound variables of a case statement.
caseBindersT :: (Monad m, ExtendPath c Crumb, ReadPath c Crumb)
                => Transform c m Ctx [Name]
caseBindersT  = caseT idR (const $ altBindersT) (const $ foldr union [])

-- Bound variables of a case statement preserved in list form
caseBindersT' :: (Monad m, ExtendPath c Crumb, ReadPath c Crumb)
                => Transform c m Ctx [[Name]]
caseBindersT'  = caseT idR (const $ altBindersT) (flip const)

-- Case scrutinee.
caseScrutR :: (Monad m, ExtendPath c Crumb, ReadPath c Crumb)
              => Rewrite c m Ctx
caseScrutR  = caseT idR (const idR) const

-- Ctxs of alts of a case statement.
caseAltCtxsT :: (Monad m, ExtendPath c Crumb, ReadPath c Crumb, AddBinders c)
                => Transform c m Ctx [Ctx]
caseAltCtxsT  = caseT idR (const $ altCtxT idR) (flip const)

-- Bind: --

-- Bound variable of a let binder.
bindBinderT :: Monad m => Transform c m Bind Name
bindBinderT  = liftT bindBinder

-- Alt: --

-- Bound variables of a case alternative.
altBindersT :: Monad m => Transform c m Alt [Name]
altBindersT  = liftT altNames

-- Tick: --

-- Body of a Tick statement.
tickBodyR :: (Monad m, ExtendPath c Crumb) => Rewrite c m Ctx
tickBodyR  = tickT idR id

-- AppD: --

-- Body of an AppD statement.
appDBodyT :: (Monad m, ExtendPath c Crumb) => Transform c m Ctx [Ctx]
appDBodyT  = appDT idR (const idR) (flip const)

-- Accepters: -----------------------------------------------------------------
{-
  - Acceptors are simple transformations that ensure the given Ctx is of
    the expected form, and fail otherwise;
  - I like to use them as they give a specific fail message at the
    start of a complex transformation if the input Ctx is invalid.
-}

varAccepterT  :: Monad m => Transform c m Ctx ()
varAccepterT   = void $ acceptWithFailMsgR isVar "not a Var."

absAccepterT  :: Monad m => Transform c m Ctx ()
absAccepterT   = void $ acceptWithFailMsgR isAbs "not an Abs."

appAccepterT  :: Monad m => Transform c m Ctx ()
appAccepterT   = void $ acceptWithFailMsgR isApp "not an App."

tickAccepterT :: Monad m => Transform c m Ctx ()
tickAccepterT  = void $ acceptWithFailMsgR isTick "not a Tick."

letAccepterT  :: Monad m => Transform c m Ctx ()
letAccepterT   = void $ acceptWithFailMsgR isLet "not a Let."

caseAccepterT :: Monad m => Transform c m Ctx ()
caseAccepterT  = void $ acceptWithFailMsgR isCase "not a case statement."

appDAccepterT :: Monad m => Transform c m Ctx ()
appDAccepterT  = void $ acceptWithFailMsgR isDatatype "not an AppD."

holeAccepterT :: Monad m => Transform c m Ctx ()
holeAccepterT  = void $ acceptWithFailMsgR isHole "not a Hole."

cVarAccepterT :: Monad m => Transform c m Ctx ()
cVarAccepterT  = void $ acceptWithFailMsgR isCVar "not a CVar."

valAccepterT  :: Monad m => Transform c m Ctx ()
valAccepterT   = void $ acceptWithFailMsgR isVal "not a Value."

redexAccepterT :: Monad m => Transform c m Ctx ()
redexAccepterT  = void $ acceptWithFailMsgR isRedex "not a redex."

funAppAccepterT :: Monad m => Transform c m Ctx ()
funAppAccepterT  = void $ acceptWithFailMsgR isFunApp "not a function application."

-- Path helper functions: -----------------------------------------------------

-- Find /a/ path to a specified variable.
specVarPathT :: ( MonadCatch m, ReadPath c Crumb
                , ExtendPath c Crumb, AddBinders c
                , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                , AddBinders (ExtendContext c (LocalPath Crumb)) )
                => Name -> Transform c m U (LocalPath Crumb)
specVarPathT  = prefixFailMsg "specVarPathT failed: "
                 . onePathToT
                 . promoteT
                 . liftT
                 . isSpecVar

-- Find /all/ paths to a specified variable.
specVarPathsT :: ( MonadCatch m, ReadPath c Crumb
                 , ExtendPath c Crumb, AddBinders c
                 , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                 , AddBinders (ExtendContext c (LocalPath Crumb)) )
                 => Name -> Transform c m U [LocalPath Crumb]
specVarPathsT  = prefixFailMsg "specVarPathsT failed: "
                  . pathsToT
                  . promoteT
                  . liftT
                  . isSpecVar

-- Find all paths to a specified /free/ variable.
specFreeVarPathsT :: ( MonadCatch m, ReadPath c Crumb
                     , ExtendPath c Crumb, AddBinders c
                     , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                     , AddBinders (ExtendContext c (LocalPath Crumb)) )
                     => Name -> Transform c m U [LocalPath Crumb]
specFreeVarPathsT ns = prefixFailMsg "specFreeVarsPathsT failed: "
                        . pathsToT
                        . promoteT
                        . transform
                        $ \c ctx -> return (isSpecVar ns ctx && isFreeContext ns c)

-- Find all paths to a specified context variable.
specCVarPathsT :: ( MonadCatch m, ReadPath c Crumb
                  , ExtendPath c Crumb, AddBinders c
                  , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                  , AddBinders (ExtendContext c (LocalPath Crumb)) )
                  => CtxKind -> Name -> Transform c m U [LocalPath Crumb]
specCVarPathsT  = prefixFailMsg "specCVarPathsT failed: "
                   . pathsToT
                   . promoteT
                   . liftT
                   .* isSpecCVar

-- Find all paths to a specified context.
specCtxPathsT :: ( MonadCatch m, ReadPath c Crumb
                 , ExtendPath c Crumb, AddBinders c
                 , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                 , AddBinders (ExtendContext c (LocalPath Crumb)) )
                 => Ctx -> Transform c m U [LocalPath Crumb]
specCtxPathsT  = prefixFailMsg "specCtxPathsT failed: "
                  . pathsToT
                  . promoteT
                  . liftT
                  . (==)

-- Find one path to a specific context.
specCtxPathT :: ( MonadCatch m, ReadPath c Crumb
                , ExtendPath c Crumb, AddBinders c
                , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                , AddBinders (ExtendContext c (LocalPath Crumb)) )
                => Ctx -> Transform c m U (LocalPath Crumb)
specCtxPathT  = prefixFailMsg "specCtxPathsT failed: "
                 . onePathToT
                 . promoteT
                 . liftT
                 . (==)

-- Find all paths to holes.
holePathsT :: ( MonadCatch m, ReadPath c Crumb
              , ExtendPath c Crumb, AddBinders c
              , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
              , AddBinders (ExtendContext c (LocalPath Crumb)) )
              => Transform c m U [LocalPath Crumb]
holePathsT  = prefixFailMsg "holePathsT failed: " $
              (++) <$> holePaths <*> (fmap (@@ CVar_Body) <$> cVarHolePaths)
              where
               holePaths     = (pathsToT . promoteT . liftT) isHole
               cVarHolePaths = (pathsToT . promoteT . liftT) isNonSubstCVar

-- Constructing ASTs for pretty printing: -------------------------------------

{-
  - Transform an AST to a ShowSetting-labelled AST;
  - We use the KURE's path variable (stored in the KureContext) and the node
    itself to apply a particular show setting.
-}

uToShowUT :: ( Monad m, ReadPath c Crumb
             , ExtendPath c Crumb, AddBinders c )
             => (AbsolutePath Crumb -> U -> ShowSettings)
             -> Transform c m U (L_U ShowSettings)
uToShowUT f = transform $ \c u -> case u of
               UGBind gbind -> L_UGBind <$> applyT (gBindToShowGBindT f) c gbind
               UCtx   ctx   -> L_UCtx   <$> applyT (ctxToShowCtxT f) c ctx
               UBind  bind  -> L_UBind  <$> applyT (bindToShowBindT f) c bind
               UAlt   alt   -> L_UAlt   <$> applyT (altToShowAltT f) c alt

gBindToShowGBindT :: ( Monad m, ReadPath c Crumb
                     , ExtendPath c Crumb, AddBinders c )
                     => (AbsolutePath Crumb -> U -> ShowSettings)
                     -> Transform c m GBind (L_GBind ShowSettings)
gBindToShowGBindT f = do
 p     <- absPath <$> contextT
 gBind <- idR
 let ss = f p (UGBind gBind)
 case gBind of
  CBind{} -> cBindT idR idR (ctxToShowCtxT f)
              (\k ns sctx -> L_CBind (k, ns, sctx, ss))
  TBind{} -> tBindT idR (ctxToShowCtxT f)
              (\ns sctx -> L_TBind (ns, sctx, ss))

ctxToShowCtxT :: ( Monad m, ReadPath c Crumb
                 , ExtendPath c Crumb, AddBinders c )
                 => (AbsolutePath Crumb -> U -> ShowSettings)
                 -> Transform c m Ctx (L_Ctx ShowSettings)
ctxToShowCtxT f = do
 p   <- absPath <$> contextT
 ctx <- idR
 let ss  = f p (UCtx ctx)
     rec = ctxToShowCtxT f
 case ctx of
  Var ns   -> return $ L_Var (ns, ss)
  LitInt i -> return $ L_LitInt (i ,ss)
  LitStr s -> return $ L_LitStr (s, ss)
  Abs{}    -> absT idR rec (L_Abs .* (, , ss))
  App{}    -> appT rec rec (L_App .* (, , ss))
  Tick{}   -> tickT rec (L_Tick . (, ss))
  Let{}    -> letT (const $ bindToShowBindT f) rec (L_Let .* (, , ss))
  Case{}   -> caseT rec (const $ altToShowAltT f) (L_Case .* (, , ss))
  AppD{}   -> appDT idR (const rec) (L_AppD .* (, , ss))
  Hole     -> return (L_Hole ss)
  CVar{}   -> cVarT idR idR rec' (\k ns msctx -> L_CVar (k, ns, msctx, ss))
              where rec' = transform $ \c mctx -> case mctx of
                            Nothing -> return Nothing
                            Just ctx  -> Just <$> applyT rec c ctx

bindToShowBindT :: ( Monad m, ReadPath c Crumb
                   , ExtendPath c Crumb, AddBinders c )
                   => (AbsolutePath Crumb -> U -> ShowSettings)
                   -> Transform c m Bind (L_Bind ShowSettings)
bindToShowBindT f = do
 p    <- absPath <$> contextT
 bind <- idR
 let ss = f p (UBind bind)
 bindT idR (ctxToShowCtxT f) idR (\ns sctx idx -> L_Bind (ns, sctx, idx, ss))

altToShowAltT :: ( Monad m, ReadPath c Crumb
                 , ExtendPath c Crumb, AddBinders c )
                 => (AbsolutePath Crumb -> U -> ShowSettings)
                 -> Transform c m Alt (L_Alt ShowSettings)
altToShowAltT f = do
 p   <- absPath <$> contextT
 alt <- idR
 let ss = f p (UAlt alt)
 altT idR (const idR) (ctxToShowCtxT f) idR
  (\con nss sctx idx -> L_Alt (con, nss, sctx, idx, ss))




-- Ensuring free variables and bound variables of statements are disjoint: -----

{-
letBvsFvsDisjointT :: ( ExtendPath c Crumb, ReadPath c Crumb
                      , AddBinders c, Monad m
                      , MonadCatch m )
                      => [Name] -> Transform c m Ctx Bool
letBvsFvsDisjointT nss = do
  ctxs <- letBindingCtxsT
  ctx  <- letBodyR
  fvs  <- constT $ freeVarsList (ctx : ctxs)
  bvs  <- letBindersT
  return (null $ fvs `intersect` bvs \\ nss)
-}
