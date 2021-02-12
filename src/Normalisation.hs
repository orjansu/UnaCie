{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Normalisation
  ( isNormalisedT      -- Check if normalised.
  , normaliseAppDR     -- Normalise a datatype.
  , normaliseAppR      -- Normalise an application.
  , normaliseNonRecR   -- Non-recursively normalise.
  , normaliseR         -- Recursively normalise.
  , normaliseRenameR   -- Recursively normalise and rename.
  , normaliseTickR     -- Normalise a tick.
  , renameMultipleR    -- Rename multiple bindings.
  , renameR            -- Recursively rename all bindings.
  , renameSingleR      -- Rename a single binding.
  , sugarAppDR         -- Sugar a datatype.
  , sugarAppR          -- Sugar an application (specific binding).
  , sugarAppR'         -- Sugar an application (single binding).
  , sugarNonRecR       -- Non-recursively sugar.
  , sugarR             -- Recursively sugar.
  , sugarTickR         -- Sugar a tick.
  ) where

import Classes     (AddBinders, SafeNames, freshVar)
import Crumb       (Crumb)
import CtxAST      (Alt(..), Bind(..), Ctx(..), Name)
import CtxUtils    ( isAppD, isAtomic, isNormDatatype
                    , isVar, reindexBinds )
import Kure        ( absTNoBind, altAllR, altTNoBind
                   , appDT,appT,  bindAllR, bindCtxT
                   , bindT, caseAllR, caseAnyR
                   , letTNoBind, tickT )
import KureContext (emptyKureContext)
import KureExtra   (any0tdR, applyAtSnocPathT, liftT)
import Subst       ( genNewBinderT', sieveT, substNonRecLetBindingsR
                   , substR, substTryListVarR, substTryVarR )
import Universes   (U(..))
import Utils       ((.*), notNull, safeTailSnocPath)
import StronglyConnectedComponents (hasSCCFromEdges')

import TransUtils
  ( absBinderT
  , altBindersT
  , appAccepterT
  , appDAccepterT
  , bindBinderT
  , deleteLetBindingR
  , deleteUnusedLetBindingsR
  , freeVarsListT
  , freeVarsT
  , freshVarBFT'
  , freshVarFT
  , freshVarT
  , letAccepterT
  , letBindersT
  , letBindingLookupT
  , letBindingsT
  , letBodyR
  , specFreeVarPathsT
  , tickAccepterT
  , tickBodyR
  )

import Control.Arrow       ((>>>))
import Control.Monad       (mapM)
import Control.Monad.State (MonadState)
import Data.List           (partition)
import Data.Monoid hiding  (Alt)
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
  , allR
  , anytdR
  , applyR
  , applyT
  , catchesM
  , changedR
  , constT
  , contextfreeT
  , crushtdT
  , extractR
  , extractT
  , guardM
  , guardMsg
  , guardMsgM
  , idR
  , mapT
  , prefixFailMsg
  , promoteR
  , promoteT
  , setFailMsg
  , tryR
  )

{-
  <TO-DO>: - Would be nice if we could have an option where all the let
             bindings introduced by normalisation were collected at the top
             of the expression (clashes permitting).
           - renameSingleUR error message is annoying on 'case'.

  Information:
  -----------------------------------------------------------------------------
  - We normalise (desugar) a context by removing all syntactic sugar,
    including ticks, non-right-atomic applications and non-atomic datatypes;
  - We also sugar, which is the opposite of normalisation;
  - This module also contains functions for renaming bindings, which are used
    when e.g., performing safe-substitution for beta-reduction.
-}

-------------------------------------------------------------------------------
-- Normalise and Rename: --
-------------------------------------------------------------------------------

-- Normalise first then rename so all binders are /unique/.
normaliseRenameR :: ( SafeNames s, ExtendPath c Crumb
                    , ReadPath c Crumb, AddBinders c, MonadState s m
                    , MonadCatch m, Injection a U )
                    => Rewrite c m a
normaliseRenameR  = prefixFailMsg "normaliseRenameR failed: " $
                    normaliseR >>> renameR

-------------------------------------------------------------------------------
-- Normalisation: --
-------------------------------------------------------------------------------
-- For each method of normalisation below, we have a transformation on U types
-- (ending UR/UT) and a transformation on injectables (ending U/R).

-- Normalise all (i.e., ticks, applications and datatypes) /recursively/: --

normaliseR :: ( SafeNames s, ExtendPath c Crumb
              , ReadPath c Crumb, AddBinders c, MonadState s m
              , MonadCatch m, Injection a U )
              => Rewrite c m a
normaliseR  = prefixFailMsg "normaliseR failed: " $
              extractR normaliseUR

normaliseUR :: ( SafeNames s, ExtendPath c Crumb, ReadPath c Crumb
               , AddBinders c, MonadState s m, MonadCatch m )
               => Rewrite c m U
normaliseUR  = setFailMsg "normaliseUR failed: already normalised." $
               anytdR normaliseNonRecUR

-- Normalise all /non-recursively/ i.e., top-level constructor only: --

normaliseNonRecR :: ( SafeNames s, ExtendPath c Crumb, ReadPath c Crumb
                    , AddBinders c, MonadState s m, MonadCatch m
                    , Injection a U ) => Rewrite c m a
normaliseNonRecR  = prefixFailMsg "normaliseNonRecR failed: " $
                    extractR normaliseNonRecUR

normaliseNonRecUR :: ( SafeNames s, ExtendPath c Crumb, MonadState s m
                     , ReadPath c Crumb, AddBinders c, MonadCatch m )
                     => Rewrite c m U
normaliseNonRecUR  = setFailMsg "normaliseNonRecUR failed: nothing to normalise." $
                     promoteR $ catchesM [ normaliseTickR
                                         , normaliseAppR
                                         , normaliseAppDR ]

-- Specific normalisations: ---------------------------------------------------

-- Normalise non-right-atomic application to a let statement.
normaliseAppR :: ( SafeNames s, MonadState s m, MonadCatch m, ReadPath c Crumb
                 , ExtendPath c Crumb, AddBinders c )
                 => Rewrite c m Ctx
normaliseAppR  = prefixFailMsg "normaliseAppR failed: " $ do
                   -- Check input is firstly an app, then non-right-atomic.
                   appAccepterT
                   App c1 c2 <- idR
                   case c2 of
                     Var{} -> fail "already normalised."
                     -- Introduce new variable and bind right arg. of app.
                     _     -> freshVarBFT' >>= \ns -> return $
                               Let [Bind ns c2 0] (App c1 $ Var ns)

-- Normalise a tick to a let statement
normaliseTickR :: ( SafeNames s, ExtendPath c Crumb, MonadState s m
                  , MonadCatch m, AddBinders c
                  , ReadPath c Crumb )
                  => Rewrite c m Ctx
normaliseTickR  = prefixFailMsg "normaliseTickR failed: " $ do
                    tickAccepterT        -- Check input is a tick.
                    ctx <- tickBodyR     -- Remove tick.
                    ns  <- freshVarBFT'  -- Make new let binding.
                    return $ Let [Bind ns ctx 0] (Var ns)

{-
  - Normalise a non-atomic datatype to a let statement;
  - I can't think of a better way of doing this currently;
  - The issue is with the binders: if we don't project them out in /front/
    of the AppD, then it looks /really/ ugly on screen.
-}
normaliseAppDR :: ( SafeNames s, ExtendPath c Crumb, MonadState s m
                  , MonadCatch m, ReadPath c Crumb, AddBinders c )
                  => Rewrite c m Ctx
normaliseAppDR  = prefixFailMsg "normaliseAppDR failed: " $ do
  appDAccepterT
  AppD con _ <- idR
  -- We need free variables from the whole expression, else
  -- the binders we introduce might capture them.
  nss <- sieveT
  (ctxs, bs) <- fmap concat . unzip <$> appDT idR
                 (const $ norm nss) (flip const)
  if null bs
  then fail "already normalised."
  -- Reindex indices here.
  else return (Let (reindexBinds bs) $ AppD con ctxs)

 -- Here we search for anything that's not a variable and
 -- pair it with a freshly generated variable. Pairs then
  -- become bindings for the let statement.
 where norm invalid = idR >>= \ctx -> case ctx of
        Var{}      -> return (ctx, [])
        AppD con _ -> do
         -- Recurse manually so binders at front of let.
         (cs, bs) <- fmap concat . unzip <$>
           appDT idR (const $ norm invalid) (flip const)
         ns <- freshVarT invalid
         -- Temporary indices, reindexed later.
         return (Var ns, bs ++ [Bind ns (AppD con cs) (-1)])
        _          -> freshVarFT invalid >>= \ns ->
                       return (Var ns, [Bind ns ctx (-1)])

-------------------------------------------------------------------------------
-- Check if normalised: --
-------------------------------------------------------------------------------

isNormalisedT :: ( SafeNames s, ExtendPath c Crumb
                 , ReadPath c Crumb, AddBinders c, MonadState s m, MonadCatch m
                 , Walker c U, Injection a U ) => Transform c m a Bool
isNormalisedT  = prefixFailMsg "isNormalisedT failed: " $
                 extractT isNormalisedUT

isNormalisedUT :: ( SafeNames s, ExtendPath c Crumb
                  , ReadPath c Crumb, AddBinders c, MonadState s m, MonadCatch m
                  , Walker c U ) => Transform c m U Bool
isNormalisedUT  = prefixFailMsg "isNormalisedUT failed: " $
                  getAll <$> (crushtdT . promoteT $ tick <+ app <+ appD)
                  where
                   tick = tickT (return $ All False) id
                   app  = appT idR (liftT isVar) (All .* flip const)
                   appD = liftT (All . isNormDatatype)

-------------------------------------------------------------------------------
-- Sugaring: --
-------------------------------------------------------------------------------
-- Sugaring is the opposite of normalisation, so here we /introduce/ ticks,
-- non-right atomic applications and non-atomic datatypes.

-- Sugar all (i.e., ticks, applications and datatypes) /recursively/: --

sugarR :: ( AddBinders (ExtendContext c (LocalPath Crumb))
          , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
          , AddBinders c, ReadPath c Crumb, ExtendPath c Crumb
          , MonadState s m, MonadCatch m, Injection a U
          , SafeNames s ) => Rewrite c m a
sugarR  = prefixFailMsg "sugarR failed: " $ extractR sugarRU

sugarRU :: ( AddBinders (ExtendContext c (LocalPath Crumb))
             , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
             , AddBinders c, ReadPath c Crumb, ExtendPath c Crumb
             , MonadState s m, MonadCatch m
             , SafeNames s ) => Rewrite c m U
sugarRU  = setFailMsg "sugarRU failed: nothing to sugar." $
           anytdR sugarNonRecUR

-- Sugar all /non-recursively/ i.e., top-level constructor only: --

sugarNonRecR :: ( AddBinders (ExtendContext c (LocalPath Crumb))
                , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                , SafeNames s, ExtendPath c Crumb, ReadPath c Crumb
                , AddBinders c, MonadState s m, MonadCatch m
                , Injection a U ) => Rewrite c m a
sugarNonRecR  = prefixFailMsg "sugarNonRecR failed: " $ extractR sugarNonRecUR


sugarNonRecUR :: ( AddBinders (ExtendContext c (LocalPath Crumb))
                 , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
                 , SafeNames s, ExtendPath c Crumb, MonadState s m
                 , ReadPath c Crumb, AddBinders c, MonadCatch m )
                 => Rewrite c m U
sugarNonRecUR  = setFailMsg "sugarNonRecUR failed: nothing to sugar." $
                 promoteR $ catchesM [ sugarTickR
                                     , sugarAppR'
                                     , sugarAppDR ]

-- Specific sugarings: --------------------------------------------------------

{-
  - Sugar a let statement of a specific form to a non-right-atomic application;
  - This works even if the let statement has other bindings too (thus in this
    instance will return a let statement);
  - We specify which binding to inline.
-}
sugarAppR :: ( AddBinders (ExtendContext c (LocalPath Crumb))
             , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
             , AddBinders c, ReadPath c Crumb, ExtendPath c Crumb
             , MonadState s m, MonadCatch m
             , SafeNames s ) => Name -> Rewrite c m Ctx
sugarAppR ns =
  prefixFailMsg "sugarAppR failed: " $

  -- Find the specified binding.
  letAccepterT >> letBindingLookupT ns >>= \case
    Nothing             -> fail "invalid binder."
    Just (Bind _ ctx _) -> do

      -- Make sure the binder /doesn't/ apper free in other bindings.
      guardMsgM (letBindingsT >>> freeVarsListT >>> liftT (ns `notElem`))
       "binder appears free inside binding(s)."

      -- Find all applied occurrences of binder in let's body.
      ps <- letTNoBind (const idR) (extractT $ specFreeVarPathsT ns) (flip const)
      guardMsg (notNull ps) "no applied occurrences of binder."

      -- Check that each applied occurence of the binder is the argument of
      -- an application.
      occurs <- letBodyR >>> sequence [ extractR (applyAtSnocPathT p idR)
                                      | p <- fmap safeTailSnocPath ps ]
      if all appArg occurs
      -- Delete the binding and inline its rhs.
      then deleteLetBindingR ns >>> substR ns ctx
      else fail "invalid applied occurrences of binder (s/be app. arg.)."

   where
    appArg (App _ (Var ns')) | ns == ns' = True
    appArg _                             = False

-- Sugar a /single/ binding let statement to a non-right-atomic application.
sugarAppR' :: ( AddBinders (ExtendContext c (LocalPath Crumb))
              , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
              , AddBinders c, ReadPath c Crumb, ExtendPath c Crumb
              , MonadState s m, MonadCatch m
              , SafeNames s ) => Rewrite c m Ctx
sugarAppR'  = prefixFailMsg "sugarAppR' failed: " $ idR >>= \case
                -- Has to be a let statement with a single binding.
                Let [Bind ns _ _] _ -> sugarAppR ns
                _ -> fail "incorrect form (s/be let x = M in N)."

-- Sugar a single binding let statement of a specific form to a tick.
sugarTickR :: MonadCatch m => Rewrite c m Ctx
sugarTickR  = prefixFailMsg "sugarTickR failed: " $
              contextfreeT $ \case
                Let [Bind ns body _] (Var ns') | ns == ns' -> do
                 -- Make sure binding isn't recursive.
                 fvs <- applyT freeVarsT emptyKureContext body
                 guardMsg (ns `notElem` fvs) "binding is recursive."
                 return (Tick body)
                _ -> fail "incorrect form (s/be let x = M in x, x fresh)."

-- Sugar a datatype
sugarAppDR :: ( AddBinders (ExtendContext c (LocalPath Crumb))
              , ReadPath (ExtendContext c (LocalPath Crumb)) Crumb
              , AddBinders c, ReadPath c Crumb, ExtendPath c Crumb
              , MonadState s m, MonadCatch m
              , SafeNames s ) => Rewrite c m Ctx
sugarAppDR
 = prefixFailMsg "sugarAppDR failed: " $ do

    -- Check we have a let.
    letAccepterT

    -- Check body is a suitable form.
    body <- letBodyR
    guardMsg (isAppD body) "let body is not a datatype."

    -- Split bindings into datatypes/elements e.g., { d = b : c } and { a = 1 }.
    (xs, ys) <- partition (\(Bind _ ctx _) -> isAppD ctx) <$> letBindingsT

    -- Get all free variables of datatype bindings and let body.
    fvs <- (++) <$> constT (applyT freeVarsListT emptyKureContext xs)
                <*> constT (applyT freeVarsT emptyKureContext body)

    -- Get all binders ref'd by fvs: we then need to make sure that none of
    -- these bindings refer to eachother;
    -- Note that zs are bindings that are not part of the datatype, now ignored,
    -- and put back at end.
    let (ws, zs) = partition (\(Bind ns _ _) -> ns `elem` fvs) ys

    -- Check for recursive bindings using SCC of graphs.
    -- Any recursion in the bindings means the let statement is /not/ (part of)
    -- a normalised datatype.
    guardMsgM (not . hasSCCFromEdges' <$> (constT $ applyT (mapT $ bindT idR
     freeVarsT idR (const .* (,))) emptyKureContext (ws ++ xs)))
     "not a datatype: bindings are (mutually) recursive."

    -- Make sure all ws are atomic so they can be used in the syntactic sugar.
    guardMsgM (constT $ applyT (and <$> (mapT $ bindCtxT $ liftT isAtomic))
      emptyKureContext ws) "datatype elements are non-atomic."

    -- Repeatedly inline the bindings until we reach a fixpoint.
    -- Note: okay here because bindings are non-recursive.
    body' <- constT $ applyR (substNonRecLetBindingsR $ ws ++ xs)
              emptyKureContext body

    -- Remove unused let bindings, put let back together.
    constT (applyR deleteUnusedLetBindingsR
           emptyKureContext $ Let (ws ++ xs) body') >>= \case
     Let bs _ -> return $ Let (reindexBinds $ bs ++ zs) body'
     -- If a let isn't returned, we know body' is returned
     _        -> if null zs
                 then return body'
                 else return $ Let (reindexBinds zs) body'

-------------------------------------------------------------------------------
-- Renaming: --
-------------------------------------------------------------------------------

-- Rename all binders to /completely/ fresh names (i.e., none that appear
-- already, whether bound or free).
renameR :: ( SafeNames s, ExtendPath c Crumb
           , Injection a U, MonadState s m
           , ReadPath c Crumb, AddBinders c
           , MonadCatch m ) => Rewrite c m a
renameR  = prefixFailMsg "renameR failed: " $ extractR renameUR

-- As above but for U.
renameUR :: ( SafeNames s, ExtendPath c Crumb
            , ReadPath c Crumb, AddBinders c
            , MonadState s m, MonadCatch m )
            => Rewrite c m U
renameUR  = prefixFailMsg "renameUR failed: " $ do
  u <- idR
  used <- sieveT
  let ctxR = (any0tdR . promoteR) (abs used <+ tel used <+ esac used)
  case u of
    UGBind{} -> allR ctxR
    UCtx{}   -> ctxR
    -- Can't rename the top-level binder here because bindings are mutually
    -- recursive, so would need the other bindings of the same let statement,
    -- we do the binding's rhs instead.
    UBind{}  -> promoteR $ bindAllR idR (extractR ctxR) idR
    UAlt{}   -> promoteR $ renameAltR used >>> altAllR idR (const idR)
                 (extractR ctxR) idR
 where
  -- Rename abs binder.
  abs used = do
    ns  <- absBinderT      -- Get current binder.
    ns' <- freshVarT used  -- Generate new one.
    -- Subst old for new in body and reconstruct abs.
    absTNoBind idR (substTryVarR ns ns') (const $ Abs ns')

  -- Rename /all/ let binders.
  tel used = do
    nss  <- letBindersT
    nss' <- constT (mapM (const $ freshVar used) nss)
    let subs = zip nss nss'
    -- Update /all/ new binders by substituting into binding bodies.
    letTNoBind (renameBindR subs) (substTryListVarR subs) Let
    where renameBindR subs i = bindAllR (return . snd $ subs !! i)
                                (substTryListVarR subs) idR
  -- Rename /all/ case binders.
  esac used = caseAllR idR (const $ renameAltR used)

  -- Rename /particular/ case alternative.
  renameAltR used = do
    nss <- altBindersT
    nss' <- constT (mapM (const $ freshVar used) nss)
    -- Update /all/ new binders by substituting into alt bodies.
    altTNoBind idR (const idR) (substTryListVarR $ zip nss nss')
     idR (\con _ ctx idx -> Alt con nss' ctx idx)

{-
  - Rename a single binder;
  - Takes two parameters: old binder and maybe a hint for the new one;
  - Note: this function will do its best to give the user what it wants,
    but if they ask for x and it's an invalid choice (i.e., would cause
    capture), then it'll give x0, x1, x2 etc. rather than failing;
  - Is it better to fail here if user can't get what they ask for?
-}
renameSingleR :: ( SafeNames s, ExtendPath c Crumb
                 , ReadPath c Crumb, AddBinders c, MonadState s m
                 , MonadCatch m, Walker c U, Injection a U )
                 => Name
                 -> Maybe Name
                 -> Rewrite c m a
renameSingleR  = prefixFailMsg "renameSingleR failed: "
                  .  extractR
                  .* renameSingleUR

-- AS above but for U.
renameSingleUR :: forall c m s .
                  ( SafeNames s, ExtendPath c Crumb
                  , ReadPath c Crumb, AddBinders c, MonadState s m
                  , MonadCatch m, Walker c U )
                  => Name
                  -> Maybe Name
                  -> Rewrite c m U
renameSingleUR old mnew =
  prefixFailMsg "renameSingleUR failed: " $ idR >>= \case
    UGBind{} -> fail "nothing to rename."
    UCtx{}   -> promoteR (abs <+ tel <+ esac)
    UBind{}  -> fail "cannot rename let binders in isolation because \
                       \they are mutually recursive."
    UAlt{}   -> promoteR renameAltR
  where

    -- Rename abs binder.
    abs = do
      ns <- absBinderT
      guardMsg (old == ns) "invalid binder."
      new <- genNewBinderT' mnew
      absTNoBind idR (substTryVarR old new) (const $ Abs new)

    -- Rename /single/ let binder.
    tel = do
      nss <- letBindersT
      -- Check specified binder is valid.
      guardMsg (old `elem` nss) "invalid binder."
      -- Gen. new binder.
      new <- genNewBinderT' mnew
      letTNoBind (const $ renameBindR new) (substTryVarR old new) Let
      where renameBindR new = do
              ns <- bindBinderT
              if ns == old
              then bindT idR (substTryVarR old new) idR (const $ Bind new)
              else bindAllR idR (substTryVarR old new) idR

    -- Rename /single/ binder in a case alt.
    esac = setFailMsg "invalid binder." $ changedR $ caseAnyR idR (const renameAltR)

    -- Rename a single binder in an alt.
    renameAltR = do
      nss <- altBindersT
      -- Check specified binder is valid.
      guardMsg (old `elem` nss) "invalid binder."
      new <- genNewBinderT' mnew
      altTNoBind idR (tryR . const (replaceBoundR new))
       (substTryVarR old new) idR Alt

      -- Find the right binder and replace.
      where replaceBoundR new = contextfreeT $ \ns -> do
                                 guardM (ns == old)
                                 return new

-- Generalise above to account for a list of renames.
renameMultipleR :: ( SafeNames s, ExtendPath c Crumb
                   , ReadPath c Crumb, AddBinders c, MonadState s m
                   , MonadCatch m, Walker c U, Injection a U )
                   => [(Name, Maybe Name)] -> Rewrite c m a
renameMultipleR subs = prefixFailMsg "renameMultipleR failed: " $ case subs of
  (sub : subs) -> uncurry renameSingleR sub >>> renameMultipleR subs
  [] -> idR
