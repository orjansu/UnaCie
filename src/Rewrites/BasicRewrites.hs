{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module BasicRewrites  
  ( 
    -- Applying/unapplying context/term definitions: --
    -- Capture avoiding and 'not capture avoiding' options.

    capAvoidApplyCtxDefR          -- Apply a context def. capture avoiding.
  , capAvoidApplyTermDefR         -- Apply a term def. capture avoiding.
  , capAvoidUnapplyCtxDefR        -- Unapply a ctx def. capture avoiding.
  , capAvoidUnapplyTermDefR       -- Unapply a term def. capture avoiding.
  , nonCapAvoidApplyCtxDefR       -- Apply a ctx def. 'not capture avoiding'.
  , nonCapAvoidApplyTermDefR      -- Apply a term def. 'not capture avoiding'.
  , nonCapAvoidUnapplyCtxDefR     -- Unapply a ctx def. 'not capture avoiding'.
  , nonCapAvoidUnapplyTermDefR    -- Unapply a term def. 'not capture avoiding'.

  , addTickR     -- Add a tick.
  , removeTickR  -- Remove a tick.
  
  ) where

import Classes     (boundVarsContext, isFreeContext)
import Crumb       (Crumb)
import CtxAST      (Ctx(..), Name, Term)
import CtxASTEq    (ctxAlphaEq)
import CtxKind     (CtxKind)
import CtxPatAST   (ctxToCtxPat)
import CtxPatMatch (eqCtxPat)
import CtxUtils    (substCtx)
import KureContext (emptyKureContext)
import KureExtra   (any0tdR, applyAtSnocPathR ,applyAtSnocPathT)
import KureMonad   (R)
import Subst       (safeBindersUR')
import TransUtils  (holePathsT, tickBodyR, freeVars, freeVarsT)
import Universes   (U(..))

import Control.Arrow ((>>>))
import Data.List  (intersect)
import Language.KURE
  ( AbsolutePath
  , applyT
  , constT
  , exposeT
  , extractR
  , extractT
  , guardM
  , guardMsgM
  , idR
  , inject
  , onetdR
  , prefixFailMsg
  , promoteT
  , setFailMsg
  )

{-
  <TO-DO>: - Generalise these from R.
           - Check if we should avoid capture when applying definitions?

  Information:
  -----------------------------------------------------------------------------
  - Basic rewrites used to transform terms:
    - apply a term/context definition;
    - unapply a term/context definition;
    - add a tick;
    - remove a tick.
-}

-- Applying/unapply defitions: ------------------------------------------------

{-
  - We perform necessary checks to ensure the contexts being substituted
    actually result in terms, i.e., the rewrite :: R Term and /not/
    T Term Ctx.
-}

-- Apply a term's definition, this is /definitional equality/.
capAvoidApplyTermDefR :: Name -> Term -> AbsolutePath Crumb -> R Term
capAvoidApplyTermDefR ns sub p = 
  prefixFailMsg "capAvoidApplyTermDefR failed: " $ do

    -- Ensure free variables in the sub won't be captured.
    -- This is done by renaming bindings in t /up front/.
    -- Note that this will rename the odd one unecessarily from
    -- time to time, but for now that's okay.
    unsafes <- constT $ freeVars sub
 
    -- Note: ns must be free here, so any renaming won't effect /it/.
    -- Just replace the first one we come to, fail if none.
    renameR unsafes >>> replaceR
  
  where 
    renameR  = extractR . any0tdR . safeBindersUR' 
    replaceR = setFailMsg ("no free occurrence of '" ++ ns ++ "\' in scope.") $
                extractR . applyAtSnocPathR p . onetdR $ freeVarReplaceR ns sub



-- As above, but doesn't take var capture of sub into consideration.
nonCapAvoidApplyTermDefR :: Name -> Term -> R Term
nonCapAvoidApplyTermDefR ns sub = 
  prefixFailMsg  "nonCapAvoidApplyTermDefR failed: " 
   . setFailMsg ("no free occurrence of '" ++ ns ++ "\' in scope.")
   . extractR 
   . onetdR 
   $ freeVarReplaceR ns sub



-- Unapply a term's definition: this is /definitional equality/.
capAvoidUnapplyTermDefR :: Name -> Term -> AbsolutePath Crumb -> R Term
capAvoidUnapplyTermDefR ns sub p = 
  prefixFailMsg "capAvoidUnapplyTermDefR failed: " $ do 

    -- Ensure free variables in the sub aren't captured.
    unsafes <- constT $ freeVars sub

    -- Note: rename any binders that will capture ns when replaced.
    renameR [ns] >>> replaceR unsafes
  
  where 
    renameR = extractR . any0tdR . safeBindersUR' 
    
    replaceR unsafes = 
      setFailMsg ("no free definition of '" ++ ns ++ "\' in scope.") 
       . extractR
       . applyAtSnocPathR p 
       . onetdR 
       $ do
           -- Project term/KURE's context
           (c, UCtx ctx) <- exposeT
           -- Make sure is alpha-equiv, and no frees are captured.
           guardMsgM (constT $ ctxAlphaEq ctx sub) "not alpha-equiv."
           guardM (null $ unsafes `intersect` boundVarsContext c)
           return (UCtx $ Var ns)


-- As above, but doesn't take var capture of sub into consideration.
nonCapAvoidUnapplyTermDefR :: Name -> Term -> R Term
nonCapAvoidUnapplyTermDefR ns sub = 
  prefixFailMsg "nonCapAvoidUnapplyTermDefR failed: " 
   . setFailMsg ("no definition of '" ++ ns ++ "\' in scope.") 
   . extractR
   . onetdR 
   $ do
       -- Project term.
       UCtx ctx <- idR
       -- Make sure is alpha-equiv.
       guardMsgM (constT $ ctxAlphaEq ctx sub) "not alpha-equiv."
       return (UCtx $ Var ns)



-- Apply a context's definition: this is /definitional equality/.
capAvoidApplyCtxDefR :: CtxKind -> Name -> Ctx -> AbsolutePath Crumb -> R Term
capAvoidApplyCtxDefR k ns sub p = 
  prefixFailMsg "capAvoidApplyCtxDefR failed: " $ do 
    
    -- Ensure free variables in the sub won't be captured.
    -- This is done by renaming bindings in t /up front/.
    -- Note that this will rename the odd one unecessarily from
    -- time to time, but for now that's okay.
    unsafes <- constT $ freeVars sub
 
    -- Note: ns relates to a CVar, so won't be renamed.
    renameR unsafes >>> replaceR
  
  where 
    renameR  = extractR . any0tdR . safeBindersUR' 
    replaceR = setFailMsg ("no substituted occurrence of '" ++ ns ++ "\' in scope.")
                . extractR 
                . applyAtSnocPathR p 
                . onetdR 
                $ cVarReplaceR k ns sub


-- As above, but doesn't take var capture of sub into consideration.
nonCapAvoidApplyCtxDefR :: CtxKind -> Name -> Ctx -> R Term
nonCapAvoidApplyCtxDefR k ns sub = 
  prefixFailMsg "nonCapAvoidApplyCtxDefR failed: " 
   . setFailMsg ("no substituted occurrence of '" ++ ns ++ "\' in scope.")
   . extractR 
   . onetdR 
   $ cVarReplaceR k ns sub



-- Unapply a context's definition: this is /definitional equality/.
-- Note: doesn't use alpha-equiv.
capAvoidUnapplyCtxDefR :: CtxKind -> Name -> Ctx -> R Term
capAvoidUnapplyCtxDefR k ns sub = 
  prefixFailMsg "capAvoidUnapplyCtxDefR failed: " $ 
    
    -- Ensure sub has holes.
    constT (applyT holePathsT emptyKureContext $ inject sub) >>= \case
         
      -- If no holes, fail (for now, because would result in 
      -- an un-subst. CVar, which we don't allow yet).
      [] -> fail "can only unapply contexts with holes." 
      (hPath : _) -> do 
        
        -- Convert sub to a pattern for matching.
        let sub' = ctxToCtxPat sub

        -- Ensure free variables in the sub aren't captured.
        unsafes <- constT $ applyT freeVarsT emptyKureContext sub

        -- Do the replacing.
        replaceR unsafes sub' hPath

 where replaceR unsafes sub' hPath = 
        setFailMsg ("no free definition of '" ++ ns ++ "\' in scope.") 
         . extractR
         . onetdR 
         $ do
             -- Project term/KURE's context
             (c, UCtx ctx) <- exposeT
             -- Make sure is match and no frees are captured. 
             guardM (eqCtxPat sub' ctx)
             guardM (null $ unsafes `intersect` boundVarsContext c)
             hSub <- extractT . applyAtSnocPathT hPath . promoteT $ idR
             return (UCtx $ CVar k ns $ Just hSub)



-- As above, but doesn't take var capture of sub into consideration.
nonCapAvoidUnapplyCtxDefR :: CtxKind -> Name -> Ctx -> R Term
nonCapAvoidUnapplyCtxDefR k ns sub = 
  prefixFailMsg "nonCapAvoidUnapplyCtxDefR failed: " $ 
    
    -- Ensure sub has holes.
    constT (applyT holePathsT emptyKureContext $ inject sub) >>= \case
         
      -- If no holes, fail (for now, because would result in 
      -- an un-subst. CVar, which we don't allow yet).
      [] -> fail "can only unapply contexts with holes." 
      (hPath : _) -> do 
        
        -- Convert sub to a pattern for matching.
        let sub' = ctxToCtxPat sub

        -- Do the replacing.
        replaceR sub' hPath

 where replaceR sub' hPath = 
        setFailMsg ("no definition of '" ++ ns ++ "\' in scope.") 
         . extractR
         . onetdR 
         $ do
             -- Project term/KURE's context
             UCtx ctx <- idR
             -- Make sure is match and no frees are captured. 
             guardM (eqCtxPat sub' ctx)
             hSub <- extractT . applyAtSnocPathT hPath . promoteT $ idR
             return (UCtx $ CVar k ns $ Just hSub)


-- Manipulating ticks: --------------------------------------------------------

addTickR :: R Ctx
addTickR  = prefixFailMsg "addTickR failed: " $ Tick <$> idR

removeTickR :: R Ctx
removeTickR  = prefixFailMsg "removeTickR failed: " tickBodyR

-------------------------------------------------------------------------------
-- Helpers: --
-------------------------------------------------------------------------------

-- Replace a /free/ variable with a given term.
freeVarReplaceR :: Name -> Term -> R U
freeVarReplaceR ns sub = do
  (c, UCtx (Var ns')) <- exposeT
  -- Make sure correct var and free.
  guardM (ns == ns')
  guardM (ns `isFreeContext` c)
  return (UCtx sub)

-- Replace a /substituted/ context variable with a given ctx;
-- Return a term by substituting context with subst. from CVar
cVarReplaceR :: CtxKind -> Name -> Ctx -> R U
cVarReplaceR k ns ctx = do
  UCtx (CVar k' ns' (Just sub)) <- idR
  -- Make sure correct var and kind
  guardM (k == k')
  guardM (ns == ns')
  return (UCtx $ substCtx sub ctx)