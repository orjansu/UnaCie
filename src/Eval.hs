{-# LANGUAGE LambdaCase #-}

module Eval
  ( eval
  , evalR
  , substLetR
  ) where 

import Classes   (addLetBinders)
import Crumb     (Crumb(..))
import CtxAST    (Bind(..), Ctx(..))
import Kure      ( absAllR, appAnyR, appDAnyR'
                 , altAllR, bindAllR, caseAllR
                 , cVarAllR, tickAllR,  letAllR
                 )
import KureMonad (R)
import Subst     (substCaseR, substTryListR, substTryR)
import Universes (U)

import Language.KURE ( (<+), applyR, extractR
                     , idR, pathT, promoteR
                     , repeatR, rewrite
                     )

{-
  Information:
  -----------------------------------------------------------------------------
  - An /experimental/ evaluation function using the KURE framework (cf. the
    abstract machine);
  - This is /definitely not/ lazy evaluation (yet), because I can't work out 
    the precise semantics of KURE's transformation strategies and the 
    congruence combinators I've encoded.
-}

eval :: R Ctx
eval  = repeatR evalR

evalR :: R Ctx 
evalR  = idR >>= \case 
  Abs{}  -> absAllR idR evalR
  App (Abs ns _) arg -> extractR $ pathT [App_Fun, Abs_Body] 
                         (promoteR $ substTryR ns arg :: R U)
  App{}  -> appAnyR evalR evalR
  Tick{} -> tickAllR evalR
  AppD{} -> appDAnyR' (const evalR)
  Let{}  -> letAllR (const $ bindAllR idR evalR idR) idR 
             <+ substLetR
  Case{} -> caseAllR idR (const $ altAllR idR (const idR) evalR idR) 
             <+ caseAllR evalR (const idR)
             <+ substCaseR
  CVar{} -> cVarAllR idR idR evalR
  _      -> fail "nothing to evaluate"

{-
  Substitution for mututally recursive let bindings:

  Given:

    let f_1 = v_1
        f_2 = v_2
    in e

  Then:
    
  (1) For each v_i we substitute each f_i with "let {..} in f_i",
      that is: res_i = v_i [for all i, f_i := let {..} in f_i]
      where {..} is the full set of binders from the original 
      let statement (f_1 = v_2, f_2 = v_2 in our case)
  (2) We then substitute each f_i for res_i in e, that is:
      e [for all i, f_i := res_i]
-}

substLetR :: R Ctx 
substLetR  = rewrite $ \c (Let bs ctx) -> do
  -- Substitute each binder for its corresponding let in each value
  let (binders, to_sub, new_lets) = unzip3 $ fmap (extract bs) bs      
  let subs = zip binders new_lets                                     
                                                                        
  -- Substitute new lets for each binding occurrence in values         
  new_vals <- mapM (applyR (substTryListR subs) c) to_sub              
  -- Substitute binding occurrences in e for new values              
  let val_subs = zip binders new_vals                               
  applyR (substTryListR val_subs) (addLetBinders bs c) ctx            
  
  where extract bs (Bind ns ctx _) = (ns, ctx, Let bs (Var ns))       