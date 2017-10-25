{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module TickAlgebraRewrites where

import CtxAST
import CtxPatAST
import KureMonad
import KureContext
import Eval
import Kure
import CtxASTEq
import CtxUtils
import CtxEqLib
import CtxKind
import CtxGen
import CtxPatMatch
import CtxCheck
import TransUtils
import Subst
import KureExtra
import Utils
import Classes
import Universes

import Language.KURE
import Control.Arrow
import Data.Tuple.Select
import Data.List
import Data.Maybe
import qualified Data.Map as Map


{-
  <TO-DO>: - These should be generalised from R
           - uncaseEvalR is hacky
           - make ungcAllR split binders if let will capture
           - unLetFlattenR accepts [Name], but cmd parser can't provide
             that yet

  Information:
  -----------------------------------------------------------------------------
  - Rewrites relating to the laws of M&S' Tick Algebra.

  Working notes:
  -----------------------------------------------------------------------------
  - We have two rewrites per tick law, one for each direction:
    - Left to right is just the name of the law;
    - Right to left has an 'un' prefix.
  - The laws defined here are designed to be standalone, i.e., make no 
    assumptions about their arguments, regardless of how they are used within
    the system (unless /specifically/ stated otherwise).
-}

-------------------------------------------------------------------------------
-- beta: --
-------------------------------------------------------------------------------

-- (\x.M) y <~> M[x:= y]
betaR :: R Ctx 
betaR  = prefixFailMsg "betaR failed: " $ redexAccepterT >> evalBetaR

-- Generalisation of above that allows curried functions applied to multiple
-- arguments to be evaluated in one go.
-- Only accepts applications whereby the number of arguments <= the arity 
-- of the function (i.e., partial/full application is allowed).
betaAllR :: R Ctx 
betaAllR  = prefixFailMsg "betaAllR failed: " $ do 
  -- Check is function application.
  funAppAccepterT
  funApp <- idR
  -- Check number of arguments and number of abstractions.
  let i = numArgs funApp
  let j = numAbs (fun funApp)
  -- Only accept partial or full application.
  guardMsg (i <= j) "invalid function application [too many arguments]."
  -- Consume all given arguments.
  andR $ replicate i evalBetaR

-- M[x:= a, y:= b, ..] <~> (\x.\y. .. M) a b ..
-- A generalised version of beta-expansion.
unbetaR :: Ctx -> R Ctx 
unbetaR ctx = prefixFailMsg "unbetaR failed: " $ 
  if isFunApp ctx 
  then do 
    -- Beta reduce the parameter to check compatability.
    reduct <- prefixFailMsg "parameter error: " $ constT $
     applyR betaAllR emptyKureContext ctx
    -- Check reduct is alpha-equiv.
    ctx <$ guardMsgM (contextfreeT $ ctxAlphaEq reduct) "parameter error: \
     \incompatible function application."
  else fail "parameter error: not a function application."



-------------------------------------------------------------------------------
-- case beta: --
-------------------------------------------------------------------------------

-- Note: in this case it's safe for us to use substCaseR because case 
-- reduction cannot encode general recursion, therefore it \must\ terminate.

-- case c_j y_ of { c_i x_i_ -> M_i } <~> M_j [x_j_:= y_]
caseBetaR :: R Ctx 
caseBetaR  = prefixFailMsg "caseBetaR failed: " $ caseAccepterT >> substCaseR

--  M_j [x_j:= y] <~> case c_j y of { c_i x_i -> M_i }
uncaseBetaR :: Ctx -> R Ctx 
uncaseBetaR ctx = prefixFailMsg "uncaseBaseR failed: " $ do 
  if isCase ctx 
  then do 
    -- Case reduce the parameter to check compatability.
    reduct <- prefixFailMsg "parameter error: " $ constT $ 
     applyR substCaseR emptyKureContext ctx
    -- Check reduct is alpha-equiv.
    ctx <$ guardMsgM (contextfreeT $ ctxAlphaEq reduct) "parameter error: \
     \incompatible case statement."
  else fail "parameter error: not a case statement."































































-- ######### CHECKED #########
-- gc: ------------------------------------------------------------------------
-- let { x_ = M_} in N <~> N, x_ disjoint from N and rest of bindings

-- Single unused binding.
gcR :: Name -> R Ctx 
gcR  = prefixFailMsg "gcR failed: " . deleteUnusedLetBindingR 

-- All unused bindings.
gcAllR :: R Ctx 
gcAllR  = prefixFailMsg "gcAllR failed: " $ deleteUnusedLetBindingsR



-- ######### CHECKED #########
-- ungc: ----------------------------------------------------------------------
-- N <~> let { x_ = M_} in N, x_ disjoint from N and rest of bindings
ungcR   :: [Bind] -> R Ctx 
ungcR bs = prefixFailMsg "ungcR failed: " $ do 
  fvs <- liftContext nullBinders freeVarsT
  let bvs = fmap bindBinder bs
  guardMsg (null $ fvs `intersect` bvs)
    "binders cause variable capture."
  
  -- Make sure added binders dont' ref. eachother.
  fvss <- fmap concat . groupOthers <$> (constT $ applyT 
           (mapT $ bindCtxT freeVarsT) emptyKureContext bs)
  let bs' = fmap fst $ filter f (zip bs fvss) 

  -- If single bindings, bs' will be null so this is an edge case
  if notNull bs' && bs' /= bs  
  then fail "binders are mutually recursive."

  -- Try and merge with existing binders, if a let statement.
  else idR >>= \case 
    tel@(Let bs_ body_) -> do 
     fvsBs <- constT $ applyT (concatMapT $ bindCtxT freeVarsT) emptyKureContext bs
     if null $ (fvsBs ++ bvs) `intersect` fmap bindBinder bs_ 
     then return $Let (reindexBinds bs ++ bs_) body_
     else return $ Let bs tel
    ctx  -> return $ Let bs ctx
  
  where f (Bind bs _ _, fvs) = bs `elem` fvs


-- ######### CHECKED #########
-- let flatten: ---------------------------------------------------------------
-- let { x_ = L_ } in let { y_ = M_ } in N <~> let { x_ = L_, y_ = M_ } in N
-- y_ disjoint from x_ and FV(L)

letFlattenR :: R Ctx 
letFlattenR  = prefixFailMsg "letFlattenR failed: " $ idR >>= \case 
  Let _ Let{} -> do 
    fvs <- letBindingCtxsT >>> concatMapT (liftContext nullBinders freeVarsT)
    bvs <- letBindersT 
    letT (const idR) (safeBindersR $ bvs ++ fvs) (\bs1 (Let bs2 body) ->
      Let (reindexBinds $ bs1 ++ bs2) body)
  _ -> fail "not a nested let."


-- ######### CHECKED #########
-- unlet flatten: -------------------------------------------------------------
-- let { x_ = L_, y_ = M_ } in N <~> let { x_ = L_ } in let { y_ = M_ } in N

unletFlattenR :: [Name] -> R Ctx 
unletFlattenR bvs = prefixFailMsg "letFlattenR failed: " $ do 
  letAccepterT
  guardMsgM ((bvs' ==) . (bvs' `intersect`) <$> letBindersT) "invalid binders."
  (l, r) <- partition partBinds <$> letBindingsT
  guardMsg (notNull l && notNull r) "less binders required."
  lFvs <- constT $ applyT (concatMapT freeVarsT) emptyKureContext l 
  let rBvs = fmap bindBinder r
  guardMsg (null $ lFvs `intersect` rBvs) "splitting would introduce free\ 
    \ variables."
  (Let (reindexBinds l) . Let (reindexBinds r)) <$> letBodyR

  where
    partBinds (Bind bs _ _) = bs `elem` bvs'
    bvs' = nub bvs


-- ######### CHECKED #########
-- value beta: ----------------------------------------------------------------
-- let { x = V, y_ = D[x]_ } in C[x] <~> let { x = V, y_ = D['V]_ } in C ['V]

-- Generate function projects out the context options for substitition
-- and allows user to manually construct the transformation for themselves.
valueBetaGenT :: Name -> CtxEqLib -> Maybe Term -> T Ctx [Ctx]
valueBetaGenT ns lib mterm = prefixFailMsg "valueBetaGenT failed: " letGen   
  where 

    -- If let construct, attempt to generate from a local binding
    -- => Find the binding in the let statement.
    -- Note: letBindingLookuoT returns Nothing if the given term is /not/ a let 
    -- statement, so this works for other constructors too.
    letGen = letBindingLookupT ns >>= \case
      -- If binding not found or not a let statement, look in library
      Nothing -> libGen
      -- Binding found:
      Just (Bind _ body idx) -> do 

       -- Binding must have a value body
       guardMsg (isVal body) "not a value binding."

       -- For each binding, several D[x] contexts may be valid, so must 
       -- generate all possibilities, similarly for the C[x] body.
       idR >>= \(Let _ ctx) ->
             -- Generate all possible D[x]s
        letT (genBindCtxs idx) 
             -- Generate all possible C[x]s
             (fmap sel1 <$> letGenCtxs) 
             -- We take all combinations of the generated options;
             -- We have manually added the each binding b (see genBindCtxs),
             -- and the let body ctx as well, as can't assume everything is being
             -- subbed. Because of this, the first option in the list is precisely
             -- the argument, so we drop it because it's a 'useless' option.
             (\bss ctxs -> drop 1 [ Let bs' ctx' | bs' <- sequence bss, ctx' <- ctx : ctxs ])
    
    -- If other construct or let without correct binding, attempt 
    -- to generate from a library binding.
    libGen = case mterm of 
      Nothing -> fail "invalid binder."
      Just term -> do 
        -- Make sure binding's body from library is a value.
        guardMsg (isVal term) "not a value binding."
        -- Now we just generate contexts over the term itself, because we assume
        -- we have let { <lib_binding> } in term. 
        fmap sel1 <$> libGenCtxs

    genBindCtxs :: Int -> Int -> T Bind [Bind]
    genBindCtxs idx idx' 
     | idx == idx' = return <$> idR
     | otherwise   = idR >>= \b@(Bind ns _ idx) -> bindT mempty letGenCtxs mempty 
                      (\() xs () -> b : fmap (flip (Bind ns) idx . sel1) xs)

    -- Here we just ensure that x is not bound in the generated context D/C,
    -- as we know it's by bound the binding x = V.
    letGenCtxs = genCtxsFvBvDisjoint (specSubstGen (Var ns)) [STD] lib

    -- Here, not only do we have to ensure that x is not bound in the generated
    -- context C, we also have to ensure that it is /free/ in the current scope.
    libGenCtxs = genCtxsFvBvDisjointFreeSubstCurrScope (specSubstGen (Var ns)) [STD] lib 


-- Match function allows user to input a context pattern to guide the 
-- construction  of the transformation. We piggyback using the valueBetaGenT 
-- function for now.
valueBetaMatchT :: Name -> CtxEqLib -> Maybe Term -> CtxPat -> T Ctx [Ctx]
valueBetaMatchT ns lib mterm pctx = prefixFailMsg "valueBetaMatchT failed: " $ do 
  -- All valid options.
  gens <- valueBetaGenT ns lib mterm
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return (gens `intersect` matches)

-- Standard function requires a compatible context to use for substitutions
-- Note that variable capture as a result of substitution is valid.
valueBetaR :: Name -> Ctx -> CtxEqLib -> Maybe Term -> R Ctx 
valueBetaR ns sub lib mterm = prefixFailMsg "valueBetaR failed: " $ do 

  -- Check the specified context is valid, i.e., element of the valid
  -- ones generated, we store the result here for now in case we can give a 
  -- better error message /first/.
  isValidCtx <- (sub `elem`) <$> valueBetaGenT ns lib mterm

  -- Find the let binding if a let statement, or use the lib term if not.
  letBindingLookupT ns >>= \case
    Nothing -> case mterm of
      -- Binder appears nowhere, so invalid.
      Nothing -> fail "invalid binder."
      Just term -> do 
        -- Check the term is a value.
        guardMsg (isVal term) "not a value binding."
        -- Check the term is not bound in the current KURE context.
        guardMsgM ((ns `notElem`) <$> boundVarsContextT) $ "'" ++ ns ++ 
         "' is bound in current scope."
        guardMsg isValidCtx "invalid context"
        constT $ applyR (capAvoidSubstCtxR $ Tick term) emptyKureContext sub
    Just (Bind _ body _) -> do
      guardMsg (isValidCtx) "invalid context"
      constT $ applyR (letAllR (const $ substBindR body)
        -- We use capture avoiding context substitution to ensure 
        -- that no free variables in V are captured in D/C 
        (tryR . capAvoidSubstCtxR' $ Tick body)) emptyKureContext sub
  
  where substBindR body = do 
          ns' <- bindBinderT
          if ns == ns'
          then idR 
          else bindAllR idR (tryR . capAvoidSubstCtxR' $ Tick body) idR



-- ######### CHECKED #########
-- unvalue beta: --------------------------------------------------------------
-- let { x = V, y_ = D['V]_ } in C['V] <~> let { x = V, y_ = D[x]_ } in C[x]
-- Note: very similar implementation to above.

-- Generate function projects out the context options for substitition
-- and allows user to manually construct the transformation for themselves.
unvalueBetaGenT :: Name -> CtxEqLib -> Maybe Term -> T Ctx [Ctx]
unvalueBetaGenT ns lib mterm = prefixFailMsg "unvalueBetaGenT failed: " letGen   
  
  where 
    -- If let construct, attempt to generate from a local binding.
    letGen = letBindingLookupT ns >>= \case
      Nothing -> libGen
      Just (Bind _ body idx) -> do 
        guardMsg (isVal body) "not a value binding."
        idR >>= \(Let _ ctx) -> letT (genBindCtxs (Tick body) idx) 
          (fmap sel1 <$> letGenCtxs (Tick body)) 
          -- Again need to drop 1 as first defn. is precisely the term.
          (\xss ys -> drop 1 [ Let bs' ctx' | bs' <- sequence xss, ctx' <- ctx : ys ])
    
    -- If not, attempt to gen. from library binding.
    libGen = case mterm of 
      Nothing -> fail "invalid binder."
      Just term -> do 
        guardMsg (isVal term) "not a value binding."
        fmap sel1 <$> libGenCtxs (Tick term)

    genBindCtxs :: Ctx -> Int -> Int -> T Bind [Bind]
    genBindCtxs ctx idx idx' 
     | idx == idx' = return <$> idR
     | otherwise   = idR >>= \b@(Bind ns _ idx) -> bindT mempty (letGenCtxs ctx) mempty 
                      (\() xs () -> b : fmap (flip (Bind ns) idx . sel1) xs)

    -- Here we ensure that free variables in V are not bound 
    -- in the generated context D/C
    letGenCtxs ctx = genCtxsFvBvDisjoint (specSubstGen ctx) [STD] lib

    -- Here, not only do we have to ensure that free variables in V are not 
    -- bound in the generated context C, we also have to ensure that they are
    -- /free/ in the current scope.
    libGenCtxs ctx = genCtxsFvBvDisjointFreeSubstCurrScope (specSubstGen ctx) [STD] lib 


-- Match function allows user to input a context pattern to guide the 
-- construction  of the transformation. We piggyback using the unvalueBetaGenT 
-- function for now.
unvalueBetaMatchT :: Name -> CtxEqLib -> Maybe Term -> CtxPat -> T Ctx [Ctx]
unvalueBetaMatchT ns lib mterm pctx = prefixFailMsg "unvalueBetaMatchT failed: " $ do 
  -- All valid options.
  gens <- unvalueBetaGenT ns lib mterm
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return (gens `intersect` matches)


-- Standard function requires a compatible context to use for substitutions
-- Note that variable capture as a result of substitution is valid.
unvalueBetaR :: Name -> Ctx -> CtxEqLib -> Maybe Term -> R Ctx 
unvalueBetaR ns sub lib mterm = prefixFailMsg "valueBetaR failed: " $ do 
  
  -- Check the specified context is valid, i.e., element of the valid
  -- ones generated, we store the result here for now in case we can give a 
  -- better error message /first/.
  isValidCtx <- (sub `elem`) <$> unvalueBetaGenT ns lib mterm

  -- Find the let binding if a let statement or use the lib term if not.
  letBindingLookupT ns >>= \case
    Nothing -> case mterm of
      -- Binder appears nowhere, so invalid.
      Nothing -> fail "invalid binder."
      Just term -> do 
        -- Check the term is a value.
        guardMsg (isVal term) "not a value binding."
        -- Check no 
        fvs <- constT $ freeVars term
        bvs <- boundVarsContextT
        guardMsg (null $ fvs `intersect` bvs) $ "free variables in defn. of '"
          ++ ns ++ "' are bound in current scope."
        guardMsg isValidCtx "invalid context."
        constT $ applyR (capAvoidSubstCtxR $ Var ns) emptyKureContext sub
    Just{} -> constT $ applyR (letAllR (const $ substBindR) 
                                       (tryR . capAvoidSubstCtxR' $ Var ns))
                              emptyKureContext sub
         
  where substBindR = do 
          ns' <- bindBinderT
          if ns == ns'
          then idR 
          else bindAllR idR (tryR . capAvoidSubstCtxR' $ Var ns) idR


-- var beta: ----------------------------------------------------------------
-- let { x = z, y_ = D[x]_ } in C[x] ~> let { x = z, y_ = D[z]_ } in  [z]

-- Generate function projects out the context options for substitition
-- and allows user to manually construct the transformation for themselves.
varBetaGenT :: Name -> CtxEqLib -> Maybe Term -> T Ctx [Ctx]
varBetaGenT ns lib mterm = prefixFailMsg "varBetaGenT failed: " letGen   
  where 

    -- If let construct, attempt to generate from a local binding
    -- => Find the binding in the let statement.
    -- Note: letBindingLookuoT returns Nothing if the given term is /not/ a let 
    -- statement, so this works for other constructors too.
    letGen = letBindingLookupT ns >>= \case
      -- If binding not found or not a let statement, look in library
      Nothing -> libGen
      -- Binding found:
      Just (Bind _ body idx) -> do 

       -- Binding must have a variable body
       guardMsg (isVar body) "not a variable binding."

       -- For each binding, several D[x] contexts may be valid, so must 
       -- generate all possibilities, similarly for the C[x] body.
       idR >>= \(Let _ ctx) ->
             -- Generate all possible D[x]s
        letT (genBindCtxs idx) 
             -- Generate all possible C[x]s
             (fmap sel1 <$> letGenCtxs) 
             -- We take all combinations of the generated options;
             -- We have manually added the each binding b (see genBindCtxs),
             -- and the let body ctx as well, as can't assume everything is being
             -- subbed. Because of this, the first option in the list is precisely
             -- the argument, so we drop it because it's a 'useless' option.
             (\bss ctxs -> drop 1 [ Let bs' ctx' | bs' <- sequence bss, ctx' <- ctx : ctxs ])
    
    -- If other construct or let without correct binding, attempt 
    -- to generate from a library binding.
    libGen = case mterm of 
      Nothing -> fail "invalid binder."
      Just term -> do 
        -- Make sure binding's body from library is a variable.
        guardMsg (isVar term) "not a variable binding."
        -- Now we just generate contexts over the term itself, because we assume
        -- we have let { <lib_binding> } in term. 
        fmap sel1 <$> libGenCtxs

    genBindCtxs :: Int -> Int -> T Bind [Bind]
    genBindCtxs idx idx' 
     | idx == idx' = return <$> idR
     | otherwise   = idR >>= \b@(Bind ns _ idx) -> bindT mempty letGenCtxs mempty 
                      (\() xs () -> b : fmap (flip (Bind ns) idx . sel1) xs)

    -- Here we just ensure that x is not bound in the generated context D/C,
    -- as we know it's by bound the binding x = V.
    letGenCtxs = genCtxsFvBvDisjoint (specSubstGen (Var ns)) [STD] lib

    -- Here, not only do we have to ensure that x is not bound in the generated
    -- context C, we also have to ensure that it is /free/ in the current scope.
    libGenCtxs = genCtxsFvBvDisjointFreeSubstCurrScope (specSubstGen (Var ns)) [STD] lib 



-- Match function allows user to input a context pattern to guide the 
-- construction  of the transformation. We piggyback using the valueBetaGenT 
-- function for now.
varBetaMatchT :: Name -> CtxEqLib -> Maybe Term -> CtxPat -> T Ctx [Ctx]
varBetaMatchT ns lib mterm pctx = prefixFailMsg "varBetaMatchT failed: " $ do 
  -- All valid options.
  gens <- varBetaGenT ns lib mterm
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return (gens `intersect` matches)

-- Standard function requires a compatible context to use for substitutions
-- Note that variable capture as a result of substitution is valid.
varBetaR :: Name -> Ctx -> CtxEqLib -> Maybe Term -> R Ctx 
varBetaR ns sub lib mterm = prefixFailMsg "varBetaR failed: " $ do 

  -- Check the specified context is valid, i.e., element of the valid
  -- ones generated, we store the result here for now in case we can give a 
  -- better error message /first/.
  isValidCtx <- (sub `elem`) <$> varBetaGenT ns lib mterm

  -- Find the let binding if a let statement, or use the lib term if not.
  letBindingLookupT ns >>= \case
    Nothing -> case mterm of
      -- Binder appears nowhere, so invalid.
      Nothing -> fail "invalid binder."
      Just term -> do 
        -- Check the term is a value.
        guardMsg (isVar term) "not a variable binding."
        -- Check the term is not bound in the current KURE context.
        guardMsgM ((ns `notElem`) <$> boundVarsContextT) $ "'" ++ ns ++ 
         "' is bound in current scope."
        guardMsg isValidCtx "invalid context"
        constT $ applyR (capAvoidSubstCtxR $ term) emptyKureContext sub
    Just (Bind _ body _) -> do
      guardMsg isValidCtx "invalid context"
      constT $ applyR (letAllR (const $ substBindR body)
        -- We use capture avoiding context substitution to ensure 
        -- that no free variables in V are captured in D/C 
        (tryR . capAvoidSubstCtxR' $ body)) emptyKureContext sub
  
  where substBindR body = do 
          ns' <- bindBinderT
          if ns == ns'
          then idR 
          else bindAllR idR (tryR . capAvoidSubstCtxR' $ body) idR


-- ######### CHECKED #########
-- let float val: -------------------------------------------------------------
-- C[let { y_ = V_ } in M] <~> let { y_ = V_ } in C[M]

-- Generate function projects out the context options for substitition
-- and allows user to manually construct the transformation for themselves.
letFloatValGenT :: CtxEqLib -> T Ctx [(Ctx, Term)]
letFloatValGenT lib = prefixFailMsg "letFloatValGenT failed: " $ 
  -- Generate all standard contexts/substitution pairs, such that the
  -- substitution is a let statement that has /all/ value bindings (genForm);
  -- Project out all the free variables from the let's bindings, and ensure
  -- they are not bound by the generated context (projFvsT).
  genCtxsSpecFreeSubstNewScope genForm freeVarsLetBindings [STD] lib >>> liftT genPairs 
  where genForm :: Maybe Ctx -> Bool 
        genForm (Just t@(Let bs _)) = isTerm t && all isVal (bindsCtxs bs)
        genForm _ = False


-- Match function allows user to input a context pattern to guide the 
-- construction  of the transformation. We piggyback using the letFloatValGenT 
-- function for now.
letFloatValMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, Term)]
letFloatValMatchT lib pctx = prefixFailMsg "letFloatValMatchT failed: " $ do 
  -- All valid options.
  gens <- letFloatValGenT lib 
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return $ filter (\(ctx, _) -> ctx `elem` matches) gens
  

-- Standard function requires a compatible context to use for substitutions.
letFloatValR :: CtxEqLib -> Ctx -> R Ctx 
letFloatValR lib ctx = prefixFailMsg "letFloatValR failed: " $
  -- Check given context is valid.
  lookup ctx <$> letFloatValGenT lib >>= \case
    Just sub -> do 
      -- Get all variables in the context (bound and free). 
      vars <- constT $ applyT sieveT emptyKureContext ctx
      -- Rename any binders that clash with them
      Let bs body <- constT $ safeBinders' vars sub
      -- Perform the substitution.
      return (Let bs $ substCtx body ctx)
    _ -> fail "invalid context."



-- ######### CHECKED #########
-- unlet float val: -----------------------------------------------------------
--  let { y_ = V_ } in C[M] <~> C[let { y_ = V_ } in M]

-- Generate function projects out the context options for substitition
-- and allows user to manually construct the transformation for themselves.
unletFloatValGenT :: CtxEqLib -> T Ctx [(Ctx, Term)]
unletFloatValGenT lib = prefixFailMsg "unletFloatValGenT: " . 
  -- At this point, we want to reset the binders in KURE's context, 
  -- because we don't want to take into consideration anything above
  -- this in the AST.
  liftContext nullBinders $ do

  -- Check let statement has only value bindings.
  letAccepterT
  letValueBindingsCheckT
  -- Generate contexts whose free variables are not bound by the let's binders.
  letBodyR >>> genFreeCtxsCurrScope termOnlySubst [STD] lib >>> liftT genPairs


-- Match function allows user to input a context pattern to guide the 
-- construction  of the transformation. We piggyback using the unletFloatValGenT 
-- function for now.
unletFloatValMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, Term)]
unletFloatValMatchT lib pctx = prefixFailMsg "unletFloatValMatchT: " $ do 
  -- All valid options.
  gens <- unletFloatValGenT lib 
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return $ filter (\(ctx, _) -> ctx `elem` matches) gens


-- Standard function requires a compatible context to use for substitutions.
unletFloatValR :: CtxEqLib -> Ctx -> R Ctx 
unletFloatValR lib ctx = prefixFailMsg "unletFloatValR failed: " .
   -- At this point, we want to reset the binders in KURE's context, 
   -- because we don't want to take into consideration anything above
   -- this in the AST.
  liftContext nullBinders $ do
   -- Check given context is valid.
   lookup ctx <$> unletFloatValGenT lib >>= \case 
     Just sub -> do
       -- Extract the let bindings.
       bs <- letBindingsT
       -- Get the free variables from the let's bindings to make sure
       -- they don't get captured by the context.
       -- Also get the let's binders, so we make sure the binders of the
       -- context are unique.
       fvs <- freeVarsLetBindings
       bvs <- letBindersT
       -- Make a temporary result without the let bindings, so we can rename
       -- any binders that will cause capture/to be disjoint.
       -- Finally, rename any conflicting binders from the context, and restore 
       -- the bindings.
       let temp = substCtx (Let [] sub) ctx
       constT $ applyR ((extractR . any0tdR . safeBindersUR' $ fvs ++ bvs) >>> 
          (extractR $ any0tdR $ (promoteR $ substEmptyLetBindingsR bs :: R U))) emptyKureContext temp
     _ -> fail "invalid context."




-- tick eval: -----------------------------------------------------------------
-- E[`M] <~> `E[M]

tickEvalGenT :: CtxEqLib -> T Ctx [(Ctx, Term)]
tickEvalGenT lib = prefixFailMsg "tickEvalGenT failed: " $ 
                     liftT (genPairs . genCtxs genForm [EVAL] lib)
  where 
    genForm :: Maybe Ctx -> Bool 
    genForm (Just t@Tick{}) = isTerm t 
    genForm _ = False


tickEvalMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, Term)]
tickEvalMatchT lib pctx = prefixFailMsg "tickEvalMatchT failed: " $ 
  liftT (genPairs . filter genForm . ctxPatGen [pctx])
  where 
    genForm :: (Ctx, [Ctx], Bool) -> Bool 
    genForm (ctx, [t@Tick{}], _) = checkCtxKind lib ctx EVAL && isTerm t
    genForm _ = False 


tickEvalR :: CtxEqLib -> Ctx -> R Ctx 
tickEvalR lib ctx = prefixFailMsg "tickEvalR failed: " $ do
  gens <- tickEvalGenT lib
  case filter (\(ctx', _) -> ctx == ctx') gens of 
    (ctx, Tick sub) : _ -> return (Tick $ substCtx sub ctx)
    _ -> fail "invalid context."




-- untick eval: ---------------------------------------------------------------
-- `E[M] <~> E[`M]

untickEvalGenT :: CtxEqLib -> T Ctx [(Ctx, Term)]
untickEvalGenT lib = prefixFailMsg "untickEvalGenT failed: " $
  tickAccepterT >> (tickBodyR >>> liftT (genPairs . genCtxs termOnlySubst [EVAL] lib))


untickEvalMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, Term)]
untickEvalMatchT lib pctx = prefixFailMsg "untickEvalMatchT failed: " $ 
  tickAccepterT >> (tickBodyR >>> liftT (genPairs . filter genForm . ctxPatGen [pctx]))
  where 
    genForm :: (Ctx, [Ctx], Bool) -> Bool 
    genForm (ctx, [sub], _) = checkCtxKind lib ctx EVAL
                              && isTerm sub
    genForm _ = False 


untickEvalR :: CtxEqLib -> Ctx -> R Ctx 
untickEvalR lib ctx = prefixFailMsg "untickEvalR failed: " $ do
  gens <- untickEvalGenT lib
  case filter (\(ctx', _) -> ctx == ctx') gens of 
    (ctx, sub) : _ -> return $ substCtx (Tick sub) ctx
    _ -> fail "invalid context."




-- let eval: ------------------------------------------------------------------
-- E[let { x = M } in N] <~> let { x = M } in E[N]

letEvalGenT :: CtxEqLib -> T Ctx [(Ctx, Term)]
letEvalGenT lib = prefixFailMsg "letEvalGenT failed: " $ 
  genCtxsSpecFreeSubstNewScope genForm freeVarsLetBindings [STD] lib >>> liftT genPairs 
  where 
    genForm :: Maybe Ctx -> Bool 
    genForm (Just Let{}) = True
    genForm _ = False


letEvalMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, Term)]
letEvalMatchT lib pctx = prefixFailMsg "letEvalMatchT failed: " $ do
  -- All valid options.
  gens <- letEvalGenT lib 
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return $ filter (\(ctx, _) -> ctx `elem` matches) gens


letEvalR :: CtxEqLib -> Ctx -> R Ctx 
letEvalR lib ctx = prefixFailMsg "letEvalR failed: " $ do 
  -- Check given context is valid.
  lookup ctx <$> letEvalGenT lib >>= \case 
    Just sub@Let{} -> do
      -- Get all variables in the context (bound and free).
      vars <- constT $ applyT sieveT emptyKureContext ctx
      -- Rename any binders that clash with them.
      Let bs body <- constT $ safeBinders' vars sub
      -- Perform the substitution.
      return (Let bs $ substCtx body ctx)
    _ -> fail "invalid context."




-- unlet eval: ----------------------------------------------------------------
-- let { x = M } in E[N] <~> E[let { x = M } in N]

unletEvalGenT :: CtxEqLib -> T Ctx [(Ctx, Term)]
unletEvalGenT lib = prefixFailMsg "unletEvalGenT failed: " .
  -- At this point, we want to reset the binders in KURE's context, 
  -- because we don't want to take into consideration anything above
  -- this in the AST.
  liftContext nullBinders $ do

  letAccepterT
  -- Generate contexts whose free variables are not bound by the let's binders.
  letBodyR >>> genFreeCtxsCurrScope termOnlySubst [EVAL] lib >>> liftT genPairs


unletEvalMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, Term)]
unletEvalMatchT lib pctx = prefixFailMsg "unletEvalMatchT failed: " $ do
  -- All valid options.
  gens <- unletEvalGenT lib 
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return $ filter (\(ctx, _) -> ctx `elem` matches) gens


unletEvalR :: CtxEqLib -> Ctx -> R Ctx 
unletEvalR lib ctx = prefixFailMsg "unletEvalR failed: " .
-- At this point, we want to reset the binders in KURE's context, 
  -- because we don't want to take into consideration anything above
  -- this in the AST.
  liftContext nullBinders $

  -- Check given context is valid.
  lookup ctx <$> unletEvalGenT lib >>= \case 
    Just sub -> do 
      -- Note: context/subst. here are for let body only.
      -- Extract the let bindings.
      bs <- letBindingsT
      -- Get the free variables from the let's bindings to make sure
      -- they don't get captured by the context.
      -- Also get the let's binders, so we make sure the binders of the
      -- context are unique.
      fvs <- freeVarsLetBindings
      bvs <- letBindersT
      -- Make a temporary result without the let bindings, so we can rename
      -- any binders that will cause capture.
      let temp = substCtx (Let [] sub) ctx
      -- Finally, rename any conflicting binders from the context, and restore 
      -- the bindings.
      constT $ applyR ((extractR . any0tdR . safeBindersUR' $ fvs ++ bvs)  >>> 
         (extractR $ any0tdR $ (promoteR $ substEmptyLetBindingsR bs :: R U))) emptyKureContext temp
    _ -> fail "invalid context."



-- case eval: -----------------------------------------------------------------                    
-- E[case M of { pat_i -> N_i }] <~> case M of { pat_i -> E[N_i] }

caseEvalGenT :: CtxEqLib -> T Ctx [(Ctx, Term)]
caseEvalGenT lib = prefixFailMsg "caseEvalGenT failed: " $ 
  genCtxsSpecFreeSubstNewScope genForm (caseScrutR >>> freeVarsT) 
   [EVAL] lib >>> liftT genPairs 
  where 
    genForm :: Maybe Ctx -> Bool 
    genForm (Just t@Case{}) = isTerm t
    genForm _               = False


caseEvalMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, Term)]
caseEvalMatchT lib pctx = prefixFailMsg "caseEvalMatchT failed: " $ do
  -- All valid options.
  gens <- caseEvalGenT lib 
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return $ filter (\(ctx, _) -> ctx `elem` matches) gens
     

caseEvalR :: CtxEqLib -> Ctx -> R Ctx 
caseEvalR lib ctx = prefixFailMsg "case-eval failed: " $  
  -- Check given context is valid.
  lookup ctx <$> caseEvalGenT lib >>= \case 
    -- Here the substituted term must be a let statement.
    Just sub@Case{} -> do
      -- Get all variables in the context (bound and free). 
      vars <- constT $ applyT sieveT emptyKureContext ctx
      -- Rename any binders that will clash with them.
      Case scrut as <- constT $ safeBinders' vars sub
      -- Perform the substitution.
      return $ Case scrut $ fmap (substAlt ctx) as
    _ -> fail "invalid context."

  where substAlt ctx (Alt con ns body idx) = 
         Alt con ns (substCtx body ctx) idx



-- uncase eval: -----------------------------------------------------------------                  
-- case M of { pat_i -> E[N_i] } <~> E[case M of { pat_i -> N_i }]

uncaseEvalGenT :: CtxEqLib -> T Ctx [(Ctx, [Term])]
uncaseEvalGenT lib  = prefixFailMsg "uncaseEvalGenT failed: " .
  -- At this point, we want to reset the binders in KURE's context, 
  -- because we don't want to take into consideration anything above
  -- this in the AST.
  liftContext nullBinders $ do

    caseAccepterT
    -- All options of contexts for each case alternative.
    opts <- caseAltCtxsT >>> mapT gen
    -- Project out the ones common to /all/ alts.
    -- The lookup just gets the correct subst.
    return $ catMaybes $ [ sequence (fmap (lookup ctx) opts) >>= Just . (ctx,) 
                         | ctx <- foldr1 intersect $ fmap (fst . unzip) opts ]

  where gen = genFreeCtxsCurrScope termOnlySubst [EVAL] lib >>> liftT genPairs


uncaseEvalMatchT :: CtxEqLib -> CtxPat -> T Ctx [(Ctx, [Term])] 
uncaseEvalMatchT lib pctx = prefixFailMsg "uncaseEvalMatchT failed: " $ do 
  -- All valid options.
  gens <- uncaseEvalGenT lib 
  -- All contexts generated by pattern matching
  matches <- sel1 . unzip3 <$> (liftT $ ctxPatGen [pctx])
  -- Return their intersection.
  return $ filter (\(ctx, _) -> ctx `elem` matches) gens


-- This is pretty hacky for now.
uncaseEvalR :: CtxEqLib -> Ctx -> R Ctx
uncaseEvalR lib ctx = prefixFailMsg "uncaseEvalR failed: " .
  -- At this point, we want to reset the binders in KURE's context, 
  -- because we don't want to take into consideration anything above
  -- this in the AST.
  liftContext nullBinders $

  lookup ctx <$> uncaseEvalGenT lib >>= \case 
     Nothing   -> fail "invalid context."
     Just substs -> do 
       -- Project out alt binders for later and scrut.
       caseBinders <- caseBindersT'
       caseScrut   <- caseScrutR
       -- Project out variables that we need to avoid clashes with.
       bvs <- caseBindersT
       fvs <- caseScrutR >>> freeVarsT
       -- Use these to rename inside the body of each alt.
       -- Just put an invalid name here so nothing get's changed
       -- <TO-DO>: do this properly.
       t  <- caseAllR (return Hole) (\i -> altAllR idR (const $ return "_") 
              (return $ substs !! i) idR)
       t' <- constT $ safeBinders' (bvs ++ fvs) t
       
       -- Put it all back together.
       (constT $ applyR (caseAllR (return caseScrut) (\i -> altAllR idR 
        (\j -> return $ caseBinders !! i !! j) 
        (return $ substs !! i) idR)) emptyKureContext t') >>> (liftT $ flip substCtx ctx)

-- eval: ----------------------------------------------------------------------
-- M ~> 'M

evalInContextR :: Map.Map Name Term -> R Term 
evalInContextR lib  
 =  prefixFailMsg "eval failed: " $ 
    do 
      vs  <- sieveT
      bvs <- boundVarsContextT
      let fvs   = vs \\ bvs 
          mdefs = zip fvs $ fmap (flip Map.lookup lib) fvs
          defs  = catMaybes $ fmap (\(ns, mdef) -> maybe Nothing 
                   (Just . (ns,)) mdef) mdefs
      setFailMsg "nothing to evaluate" $ changedR $ 
        if null defs
          then Eval.eval
          else (Let (genBinds defs) <$> idR) >>> Eval.eval
    where 
     genBinds xs = fmap (\((ns, ctx), idx) -> Bind ns ctx idx) $ zip xs [0..]


-- Helpers: -------------------------------------------------------------------
                 
-- Calculate the number of arguments supplied to a (curried) function                                           
numArgs :: Ctx -> Int 
numArgs (App Abs{} _) = 1 
numArgs (App c1 _)    = 1 + numArgs c1
numArgs _             = 0

-- Project out the function from a function application application
fun :: Ctx -> Ctx 
fun (App c1 _) = fun c1 
fun ctx        = ctx

-- Calculate the number of abstractions in a (curried) function
numAbs :: Ctx -> Int 
numAbs (Abs _ ctx) = 1 + numAbs ctx
numAbs _           = 0



-- Standard check for a context/substition pair
ctxSubstCheckT :: Ctx -> Ctx -> CtxKind -> CtxEqLib -> T Ctx ()
ctxSubstCheckT ctx sub k lib
 = do 
     guardMsg (checkCtxKind lib ctx k) "the specified context is invalid."
     guardMsg (isTerm sub) "the specified substitution is invalid."
     guardMsgM (liftT (substCtx sub ctx ==)) "the specified context/substitution \
      \pair is invalid."

     
letValueBindingsCheckT :: T Ctx () 
letValueBindingsCheckT  = guardMsgM (letBindingCtxsT >>> liftT (all isVal)) 
                           "not all value bindings."


freeVarsLetBindings :: T Ctx [Name]
freeVarsLetBindings  = letT (const $ bindT idR freeVarsT idR (\_ fvs _ -> fvs))
                            idR
                            (concat .* const)