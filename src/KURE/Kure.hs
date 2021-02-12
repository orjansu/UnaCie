{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Kure
 (
   -- Congruence combinators for CtxAST: --
   -- Primed appD combinators work on datatypes 'bodies' only.

   absAllR
 , absAnyR
 , absOneR
 , absT
 , altAllR
 , altAnyR
 , altCtxT
 , altOneR
 , altT
 , appAllR
 , appAnyR
 , appDAllR
 , appDAllR'
 , appDAnyR
 , appDAnyR'
 , appDOneR
 , appDOneR'
 , appDT
 , appDT'
 , appOneR
 , appT
 , bindAllR
 , bindAnyR
 , bindCtxT
 , bindOneR
 , bindT
 , cBindAllR
 , cBindAnyR
 , cBindOneR
 , cBindT
 , cVarAllR
 , cVarAnyR
 , cVarOneR
 , cVarT
 , caseAllR
 , caseAnyR
 , caseOneR
 , caseT
 , letAllR
 , letAnyR
 , letOneR
 , letT
 , tBindAllR
 , tBindAnyR
 , tBindOneR
 , tBindT
 , tickAllR
 , tickAnyR
 , tickOneR
 , tickT
 , varR
 , varT

 -- Special congruence combinators for binder renaming rewrites: --
 -- ManBind = manually add bindings to KURE's context.
 -- NoBind  = don't add bindings.

 , absTManBind
 , absTNoBind
 , altTManBind
 , altTNoBind
 , letTManBind
 , letTNoBind

 ) where

import Classes   (AddBinders(..), addLetBinders)
import Crumb     (Crumb(..))
import CtxAST    ( Alt(..), Bind(..), Con
                 , Ctx(..), GBind(..), Name )
import CtxKind   (CtxKind)
import Universes (U(..))

import Language.KURE
  ( ExtendPath
  , MonadCatch
  , ReadPath
  , Rewrite
  , Transform
  , Walker(..)
  , (@@)
  , applyR
  , applyT
  , changedR
  , contextfreeT
  , extractR
  , idR
  , inject
  , prefixFailMsg
  , readerT
  , rewrite
  , transform
  , unwrapAnyR
  , unwrapOneR
  , wrapAnyR
  , wrapOneR
  )

{-
  <TO-DO>: Need the same for CtxPatAST?

  Information:
  -----------------------------------------------------------------------------
  - Walker instance for Universe type U -- see KURE paper for why a Universe
    type is necessary;
  - Congruence combinators for each constructor in CtxAST. These handle the
    bookkeeping so other transformations defined in terms of them do not need
    to manually interact with KURE's context.
  Working notes:
  -----------------------------------------------------------------------------
  - See KURE papers for information on all/any/one, but they basically do the
    obvious thing: succeeding if all/any/one rewrite(s) on their child node(s)
    succeed;
  - See KURE papers for information regarding congruence combinators: It's
    worth reading up in detail about them because they are a central aspect of
    /all/ transformations.
-}

-- Walker instance minimal definition is allR, other functions have default
-- implementationd which are suitable for our purposes.
instance ( ExtendPath c Crumb, ReadPath c Crumb
         , AddBinders c ) => Walker c U where
  allR  :: forall m . MonadCatch m => Rewrite c m U -> Rewrite c m U
  allR r = prefixFailMsg "allR failed: " $ rewrite $ \c u -> case u of
            UGBind gbind -> inject <$> applyR allRGBind c gbind
            UCtx   ctx   -> inject <$> applyR allRCtx   c ctx
            UBind  bind  -> inject <$> applyR allRBind  c bind
            UAlt   alt   -> inject <$> applyR allRAlt   c alt

           where

            allRGBind :: Rewrite c m GBind
            allRGBind  = readerT $ \case
                          CBind{} -> cBindAllR idR idR (extractR r)
                          TBind{} -> tBindAllR idR (extractR r)

            allRCtx   :: Rewrite c m Ctx
            allRCtx    = readerT $ \case
                          Abs{}  -> absAllR idR (extractR r)
                          App{}  -> appAllR (extractR r) (extractR r)
                          Tick{} -> tickAllR (extractR r)
                          Let{}  -> letAllR (const $ extractR r) (extractR r)
                          Case{} -> caseAllR (extractR r) (const $ extractR r)
                          AppD{} -> appDAllR idR (const $ extractR r)
                          CVar{} -> cVarAllR idR idR (extractR r)
                          _      -> idR

            allRBind  :: Rewrite c m Bind
            allRBind   = bindAllR idR (extractR r) idR

            allRAlt   :: Rewrite c m Alt
            allRAlt    = altAllR idR (const idR) (extractR r) idR

-- Congruence combinators for constructors in CtxAST: -------------------------

-- CBind: ---------------------------------------------------------------------

cBindT :: (ExtendPath c Crumb, ReadPath c Crumb, Monad m)
          => Transform c m CtxKind a1
          -> Transform c m Name a2
          -> Transform c m Ctx a3
          -> (a1 -> a2 -> a3 -> b)
          -> Transform c m GBind b
cBindT t1 t2 t3 f = transform $ \c gbind -> case gbind of
  CBind k ns body -> f <$> applyT t1 (c @@ CBind_Kind) k
                       <*> applyT t2 (c @@ CBind_Name) ns
                       <*> applyT t3 (c @@ CBind_Body) body
  _ -> fail "not a CBind."

cBindAllR :: (ExtendPath c Crumb, ReadPath c Crumb, Monad m)
             => Rewrite c m CtxKind
             -> Rewrite c m Name
             -> Rewrite c m Ctx
             -> Rewrite c m GBind
cBindAllR r1 r2 r3 = cBindT r1 r2 r3 CBind

cBindAnyR :: (ExtendPath c Crumb, ReadPath c Crumb, MonadCatch m)
             => Rewrite c m CtxKind
             -> Rewrite c m Name
             -> Rewrite c m Ctx
             -> Rewrite c m GBind
cBindAnyR r1 r2 r3 = unwrapAnyR $ cBindAllR (wrapAnyR r1)
                      (wrapAnyR r2) (wrapAnyR r3)

cBindOneR :: (ExtendPath c Crumb, ReadPath c Crumb, MonadCatch m)
             => Rewrite c m CtxKind
             -> Rewrite c m Name
             -> Rewrite c m Ctx
             -> Rewrite c m GBind
cBindOneR r1 r2 r3 = unwrapOneR $ cBindAllR (wrapOneR r1)
                      (wrapOneR r2) (wrapOneR r3)

-- TBind: ---------------------------------------------------------------------

tBindT :: (ExtendPath c Crumb, ReadPath c Crumb, Monad m)
          => Transform c m Name a1
          -> Transform c m Ctx a2
          -> (a1 -> a2 -> b)
          -> Transform c m GBind b
tBindT t1 t2 f = transform $ \c gbind -> case gbind of
                  TBind ns body -> f <$> applyT t1 (c @@ TBind_Name) ns
                                     <*> applyT t2 (c @@ TBind_Body) body
                  _ -> fail "not a TBind."

tBindAllR :: (ExtendPath c Crumb, ReadPath c Crumb, Monad m)
             => Rewrite c m Name
             -> Rewrite c m Ctx
             -> Rewrite c m GBind
tBindAllR r1 r2 = tBindT r1 r2 TBind

tBindAnyR :: (ExtendPath c Crumb, ReadPath c Crumb, MonadCatch m)
             => Rewrite c m Name
             -> Rewrite c m Ctx
             -> Rewrite c m GBind
tBindAnyR r1 r2 = unwrapAnyR $ tBindAllR (wrapAnyR r1) (wrapAnyR r2)

tBindOneR :: (ExtendPath c Crumb, ReadPath c Crumb, MonadCatch m)
             => Rewrite c m Name
             -> Rewrite c m Ctx
             -> Rewrite c m GBind
tBindOneR r1 r2 = unwrapOneR $ tBindAllR (wrapOneR r1) (wrapOneR r2)

-- Var: -----------------------------------------------------------------------

varT :: Monad m => (Name -> b) -> Transform c m Ctx b
varT f = contextfreeT $ \case
          Var ns -> return (f ns)
          _ -> fail "not a Var."

varR :: Monad m => (Name -> Name) -> Rewrite c m Ctx
varR  = varT . (Var <$>)

-- Abs: -----------------------------------------------------------------------

absT :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
        => Transform c m Name a1
        -> Transform c m Ctx a2
        -> (a1 -> a2 -> b)
        -> Transform c m Ctx b
absT t1 t2 f = transform $ \c ctx -> case ctx of
  Abs ns ctx -> f <$> applyT t1 (c @@ Abs_Name) ns
                  <*> applyT t2 (addBinders [ns] c @@ Abs_Body) ctx
  _ -> fail "not an Abs."

absAllR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
           => Rewrite c m Name
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
absAllR r1 r2 = absT r1 r2 Abs

absAnyR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
           => Rewrite c m Name
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
absAnyR r1 r2 = unwrapAnyR $ absAllR (wrapAnyR r1) (wrapAnyR r2)

absOneR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
           => Rewrite c m Name
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
absOneR r1 r2 = unwrapOneR $ absAllR (wrapOneR r1) (wrapOneR r2)

-- App: -----------------------------------------------------------------------

appT :: (ExtendPath c Crumb, Monad m)
        => Transform c m Ctx a1
        -> Transform c m Ctx a2
        -> (a1 -> a2 -> b)
        -> Transform c m Ctx b
appT t1 t2 f = transform $ \c ctx -> case ctx of
                App c1 c2 -> f <$> applyT t1 (c @@ App_Fun) c1
                               <*> applyT t2 (c @@ App_Arg) c2
                _ -> fail "not an App."

appAllR :: (ExtendPath c Crumb, Monad m)
           => Rewrite c m Ctx
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
appAllR r1 r2 = appT r1 r2 App

appAnyR :: (ExtendPath c Crumb, MonadCatch m)
           => Rewrite c m Ctx
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
appAnyR r1 r2 = unwrapAnyR $ appAllR (wrapAnyR r1) (wrapAnyR r2)

appOneR :: (ExtendPath c Crumb, MonadCatch m)
           => Rewrite c m Ctx
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
appOneR r1 r2 = unwrapOneR $ appAllR (wrapOneR r1) (wrapOneR r2)

-- Ticks: ---------------------------------------------------------------------

tickT :: (ExtendPath c Crumb, Monad m)
         => Transform c m Ctx a
         -> (a -> b)
         -> Transform c m Ctx b
tickT t f = transform $ \c ctx -> case ctx of
             Tick ctx -> f <$> applyT t (c @@ Tick_Body) ctx
             _ -> fail "not a Tick."

tickAllR :: (ExtendPath c Crumb, Monad m)
            => Rewrite c m Ctx
            -> Rewrite c m Ctx
tickAllR r = tickT r Tick

tickAnyR :: (ExtendPath c Crumb, MonadCatch m)
            => Rewrite c m Ctx
            -> Rewrite c m Ctx
tickAnyR r = unwrapAnyR $ tickAllR (wrapAnyR r)

tickOneR :: (ExtendPath c Crumb, MonadCatch m)
            => Rewrite c m Ctx
            -> Rewrite c m Ctx
tickOneR r = unwrapOneR $ tickAllR (wrapOneR r)

-- Let: -----------------------------------------------------------------------

letT :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
        => (Int -> Transform c m Bind a1)
        -> Transform c m Ctx a2
        -> ([a1] -> a2 -> b)
        -> Transform c m Ctx b
letT t1 t2 f = transform $ \c ctx -> case ctx of
  Let bs ctx -> f <$> sequence
   [ applyT (t1 idx) (addLetBinders bs c @@ Let_Bind idx) b
   | (b, idx) <- zip bs [0..] ]
                  <*> applyT t2 (addLetBinders bs c @@ Let_Body) ctx
  _ -> fail "not a Let."

letAllR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
           => (Int -> Rewrite c m Bind)
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
letAllR r1 r2 = letT r1 r2 Let

letAnyR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
           => (Int -> Rewrite c m Bind)
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
letAnyR r1 r2 = unwrapAnyR $ letAllR (wrapAnyR . r1) (wrapAnyR r2)

letOneR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
           => (Int -> Rewrite c m Bind)
           -> Rewrite c m Ctx
           -> Rewrite c m Ctx
letOneR r1 r2 = unwrapOneR $ letAllR (wrapOneR . r1) (wrapOneR r2)

-- Bind: ----------------------------------------------------------------------

bindT :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
         => Transform c m Name a1
         -> Transform c m Ctx a2
         -> Transform c m Int a3
         -> (a1 -> a2 -> a3 -> b)
         -> Transform c m Bind b
bindT t1 t2 t3 f = transform $ \c (Bind ns ctx idx) ->
  f <$> applyT t1 (c @@ Bind_Name idx) ns
    <*> applyT t2 (addBinders [ns] c @@ Bind_Body idx) ctx
    <*> applyT t3 (c @@ Bind_Idx idx) idx

-- Working on a binding's context (i.e., RHS) only
bindCtxT :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
            => Transform c m Ctx a
            -> Transform c m Bind a
bindCtxT t = bindT mempty t mempty (\() res () -> res)

bindAllR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
            => Rewrite c m Name
            -> Rewrite c m Ctx
            -> Rewrite c m Int
            -> Rewrite c m Bind
bindAllR r1 r2 r3 = bindT r1 r2 r3 Bind

bindAnyR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
            => Rewrite c m Name
            -> Rewrite c m Ctx
            -> Rewrite c m Int
            -> Rewrite c m Bind
bindAnyR r1 r2 r3 = unwrapAnyR $ bindAllR (wrapAnyR r1)
                     (wrapAnyR r2) (wrapAnyR r3)

bindOneR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
            => Rewrite c m Name
            -> Rewrite c m Ctx
            -> Rewrite c m Int
            -> Rewrite c m Bind
bindOneR r1 r2 r3 = unwrapOneR $ bindAllR (wrapOneR r1)
                     (wrapOneR r2) (wrapOneR r3)

-- Case: ----------------------------------------------------------------------

caseT :: (ExtendPath c Crumb, ReadPath c Crumb, Monad m)
         => Transform c m Ctx a1
         -> (Int -> Transform c m Alt a2)
         -> (a1 -> [a2] -> b)
         -> Transform c m Ctx b
caseT t1 t2 f  = transform $ \c ctx -> case ctx of
  Case ctx alts -> f <$> applyT t1 (c @@ Case_Scrut) ctx
                     <*> sequence [ applyT (t2 idx) (c @@ Case_Alt idx) a
                                  | (a, idx) <- zip alts [0..] ]
  _ -> fail "not a Case."

caseAllR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
            => Rewrite c m Ctx
            -> (Int -> Rewrite c m Alt)
            -> Rewrite c m Ctx
caseAllR r1 r2 = caseT r1 r2 Case

caseAnyR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
            => Rewrite c m Ctx
            -> (Int -> Rewrite c m Alt)
            -> Rewrite c m Ctx
caseAnyR r1 r2 = unwrapAnyR $ caseAllR (wrapAnyR r1) (wrapAnyR . r2)

caseOneR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
            => Rewrite c m Ctx
            -> (Int -> Rewrite c m Alt)
            -> Rewrite c m Ctx
caseOneR r1 r2 = unwrapOneR $ caseAllR (wrapOneR r1) (wrapOneR . r2)

-- Alt: -----------------------------------------------------------------------

altT :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
        => Transform c m Con a1
        -> (Int -> Transform c m Name a2)
        -> Transform c m Ctx a3
        -> Transform c m Int a4
        -> (a1 -> [a2] -> a3 -> a4 -> b)
        -> Transform c m Alt b
altT t1 t2 t3 t4 f  = transform $ \c (Alt con nss ctx idx) ->
  f <$> applyT t1 (c @@ Alt_Con) con
    <*> sequence [ applyT (t2 idx) (c @@ Alt_Name idx) ns
                 | (ns, idx) <- zip nss [0..] ]
    <*> applyT t3 (addBinders nss c @@ Alt_Body idx) ctx
    <*> applyT t4 (c @@ Alt_Idx idx) idx

-- Working on an alt's context (i.e., RHS) only
altCtxT :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
           => Transform c m Ctx a
           -> Transform c m Alt a
altCtxT t = altT mempty (const idR) t mempty (\() _ res () -> res)

altAllR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
           => Rewrite c m Con
           -> (Int -> Rewrite c m Name)
           -> Rewrite c m Ctx
           -> Rewrite c m Int
           -> Rewrite c m Alt
altAllR r1 r2 r3 r4 = altT r1 r2 r3 r4 Alt

altAnyR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
           => Rewrite c m Con
           -> (Int -> Rewrite c m Name)
           -> Rewrite c m Ctx
           -> Rewrite c m Int
           -> Rewrite c m Alt
altAnyR r1 r2 r3 r4 = unwrapAnyR $ altAllR (wrapAnyR r1) (wrapAnyR . r2)
                       (wrapAnyR r3) (wrapAnyR r4)

altOneR :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, MonadCatch m)
           => Rewrite c m Con
           -> (Int -> Rewrite c m Name)
           -> Rewrite c m Ctx
           -> Rewrite c m Int
           -> Rewrite c m Alt
altOneR r1 r2 r3 r4 = unwrapOneR $ altAllR (wrapOneR r1) (wrapOneR . r2)
                       (wrapOneR r3) (wrapOneR r4)

-- AppD: ----------------------------------------------------------------------

appDT :: (ExtendPath c Crumb, Monad m)
         => Transform c m Con a1
         -> (Int -> Transform c m Ctx a2)
         -> (a1 -> [a2] -> b)
         -> Transform c m Ctx b
appDT t1 t2 f = transform $ \c ctx -> case ctx of
  AppD con ctxs -> f <$> applyT t1 (c @@ AppD_Con) con
                     <*> sequence [ applyT (t2 idx) (c @@ AppD_Body idx) ctx
                                  | (ctx, idx) <- zip ctxs [0..] ]
  _ -> fail "not an AppD."

appDAllR :: (ExtendPath c Crumb, Monad m)
            => Rewrite c m Con
            -> (Int -> Rewrite c m Ctx)
            -> Rewrite c m Ctx
appDAllR r1 r2 = appDT r1 r2 AppD

appDAnyR :: (ExtendPath c Crumb, MonadCatch m)
            => Rewrite c m Con
            -> (Int -> Rewrite c m Ctx)
            -> Rewrite c m Ctx
appDAnyR r1 r2 = unwrapAnyR $ appDAllR (wrapAnyR r1) (wrapAnyR . r2)

appDOneR :: (ExtendPath c Crumb, MonadCatch m)
            => Rewrite c m Con
            -> (Int -> Rewrite c m Ctx)
            -> Rewrite c m Ctx
appDOneR r1 r2 = unwrapOneR $ appDAllR (wrapOneR r1) (wrapOneR . r2)

-- Working on AppD's body only, i.e., no con
-- Have all/any/one versions, needed for evaluation.. see Eval.hs.

appDT' :: (ExtendPath c Crumb, Monad m)
          => (Int -> Transform c m Ctx a)
          -> (Con -> [a] -> b)
          -> Transform c m Ctx b
appDT' t f = transform $ \c ctx -> case ctx of
  AppD con ctxs -> f con <$> sequence
   [ applyT (t idx) (c @@ AppD_Body idx) ctx
   | (ctx, idx) <- zip ctxs [0..] ]
  _ -> fail "not an AppD."

appDAllR' :: (ExtendPath c Crumb, Monad m)
             => (Int -> Rewrite c m Ctx)
             -> Rewrite c m Ctx
appDAllR' r = appDT' r AppD

appDAnyR' :: (ExtendPath c Crumb, MonadCatch m)
             => (Int -> Rewrite c m Ctx)
             -> Rewrite c m Ctx
appDAnyR' r = unwrapAnyR $ appDAllR' (wrapAnyR . r)

appDOneR' :: (ExtendPath c Crumb, MonadCatch m)
             => (Int -> Rewrite c m Ctx)
             -> Rewrite c m Ctx
appDOneR' r = unwrapOneR $ appDAllR' (wrapOneR . r)

-- CVar: ----------------------------------------------------------------------
-- Has to be /Maybe/ because might not be substituted.

cVarT :: (ExtendPath c Crumb, Monad m)
         => Transform c m CtxKind a1
         -> Transform c m Name a2
         -> Transform c m (Maybe Ctx) a3
         -> (a1 -> a2 -> a3 -> b)
         -> Transform c m Ctx b
cVarT t1 t2 t3 f = transform $ \c ctx -> case ctx of
  CVar k ns body -> f <$> applyT t1 (c @@ CVar_Kind) k
                      <*> applyT t2 (c @@ CVar_Name) ns
                      <*> applyT t3 (c @@ CVar_Body) body
  _ -> fail "not a CVar."

cVarAllR :: (ExtendPath c Crumb, Monad m)
            => Rewrite c m CtxKind
            -> Rewrite c m Name
            -> Rewrite c m Ctx
            -> Rewrite c m Ctx
cVarAllR r1 r2 r3 = rewrite $ \c ctx -> case ctx of
  CVar k ns Nothing     -> CVar <$> applyR r1 (c @@ CVar_Kind) k
                                <*> applyR r2 (c @@ CVar_Name) ns
                                <*> pure Nothing
  CVar k ns (Just body) -> CVar <$> applyR r1 (c @@ CVar_Kind) k
                                <*> applyR r2 (c @@ CVar_Name) ns
                                <*> (Just <$> applyR r3 (c @@ CVar_Body) body)
  _ -> fail "not a CVar."

-- changedR here because 'pure Nothing' always succeeds (see above)

cVarAnyR :: (ExtendPath c Crumb, MonadCatch m)
            => Rewrite c m CtxKind
            -> Rewrite c m Name
            -> Rewrite c m Ctx
            -> Rewrite c m Ctx
cVarAnyR r1 r2 r3 = changedR $ unwrapAnyR $ cVarAllR (wrapAnyR r1)
                     (wrapAnyR r2) (wrapAnyR r3)

cVarOneR :: (ExtendPath c Crumb, MonadCatch m)
            => Rewrite c m CtxKind
            -> Rewrite c m Name
            -> Rewrite c m Ctx
            -> Rewrite c m Ctx
cVarOneR r1 r2 r3 = changedR $ unwrapOneR $ cVarAllR (wrapOneR r1)
                     (wrapOneR r2) (wrapOneR r3)



-- !!## Everything below here should be used with caution ##!! --

-------------------------------------------------------------------------------
-- Special cases for binder renaming rewrite rules: --
-------------------------------------------------------------------------------
{-
  - For some transformations/rewrites involving Ctxs, we want to traverse
    into e.g., a let body /without/ adding its let binders to the context, or
    whilst adding our own bound variables etc;
  - This happens in /very/ particular circumstances and any combinators below
    should be used with caution.
-}

-- Manual binders input into the context cf. extracting them from the statement
-- Used for e.g., substituting binders: here we need to input the new ones
-- into the context to prevent them from being used further down the line.

absTManBind :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
               => Name
               -> Transform c m Name a1
               -> Transform c m Ctx a2
               -> (a1 -> a2 -> b)
               -> Transform c m Ctx b
absTManBind bs t1 t2 f = transform $ \c ctx -> case ctx of
  Abs ns ctx -> f <$> applyT t1 (c @@ Abs_Name) ns
                  <*> applyT t2 (addBinders [bs] c @@ Abs_Body) ctx
  _ -> fail "not an Abs."

letTManBind :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
               => [Name]
               -> (Int -> Transform c m Bind a1)
               -> Transform c m Ctx a2
               -> ([a1] -> a2 -> b)
               -> Transform c m Ctx b
letTManBind bss t1 t2 f = transform $ \c ctx -> case ctx of
  Let bs ctx -> f <$> sequence
   [ applyT (t1 idx) (addBinders bss c @@ Let_Bind idx) b
   | (b, idx) <- zip bs [0..] ]
                  <*> applyT t2 (addBinders bss c @@ Let_Body) ctx
  _ -> fail "not a Let."

altTManBind :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
               => [Name]
               -> Transform c m Con a1
               -> (Int -> Transform c m Name a2)
               -> Transform c m Ctx a3
               -> Transform c m Int a4
               -> (a1 -> [a2] -> a3 -> a4 -> b)
               -> Transform c m Alt b
altTManBind bss t1 t2 t3 t4 f = transform $ \c (Alt con nss ctx idx) ->
  f <$> applyT t1 (c @@ Alt_Con) con
    <*> sequence [ applyT (t2 idx) (c @@ Alt_Name idx) ns
                 | (ns, idx) <- zip nss [0..] ]
    <*> applyT t3 (addBinders bss c @@ Alt_Body idx) ctx
    <*> applyT t4 (c @@ Alt_Idx idx) idx

-- No binders input into context, used for e.g., renaming binders: ------------

absTNoBind :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
              => Transform c m Name a1
              -> Transform c m Ctx a2
              -> (a1 -> a2 -> b)
              -> Transform c m Ctx b
absTNoBind t1 t2 f = transform $ \c ctx -> case ctx of
                       Abs ns ctx -> f <$> applyT t1 (c @@ Abs_Name) ns
                                       <*> applyT t2 (c @@ Abs_Body) ctx
                       _ -> fail "not an Abs."

letTNoBind :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
              => (Int -> Transform c m Bind a1)
              -> Transform c m Ctx a2
              -> ([a1] -> a2 -> b)
              -> Transform c m Ctx b
letTNoBind t1 t2 f = transform $ \c ctx -> case ctx of
  Let bs ctx -> f <$> sequence [ applyT (t1 n) (c @@ Let_Bind n) b
                               | (b, n) <- zip bs [0..] ]
                  <*> applyT t2 (c @@ Let_Body) ctx
  _ -> fail "not a Let."

altTNoBind :: (ExtendPath c Crumb, ReadPath c Crumb, AddBinders c, Monad m)
              => Transform c m Con a1
              -> (Int -> Transform c m Name a2)
              -> Transform c m Ctx a3
              -> Transform c m Int a4
              -> (a1 -> [a2] -> a3 -> a4 -> b)
              -> Transform c m Alt b
altTNoBind t1 t2 t3 t4 f = transform $ \c (Alt con nss ctx idx) ->
  f <$> applyT t1 (c @@ Alt_Con) con
    <*> sequence [ applyT (t2 idx) (c @@ Alt_Name idx) ns
                 | (ns, idx) <- zip nss [0..] ]
    <*> applyT t3 (c @@ Alt_Body idx) ctx
    <*> applyT t4 (c @@ Alt_Idx idx) idx
