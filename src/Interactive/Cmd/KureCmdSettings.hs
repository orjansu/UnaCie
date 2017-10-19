
module KureCmdSettings where

import CmdAST    (CmdName)
import Relations (Relation(..))
import Types     (Matcher, Refiner, Interp)
import Utils     ((.*))

-- External matchers/refiners/interpreters
import qualified BasicKureCmd
import qualified MiscKureCmd
import qualified TickKureCmd
import qualified ListKureCmd
import qualified WorkerWrapperKureCmd
import qualified TabulateKureCmd

import qualified Data.Map as Map

{-
  <TO-DO>: N/A

  Information:
  -----------------------------------------------------------------------------
  - Settings file for KURE commands.
-}

data KureCmdSetting   =  KureCmdSetting { rel      :: Relation
                                        , matcher  :: Matcher
                                        , refiner  :: Refiner
                                        , interp   :: Interp
                                        }
type KureCmdSettings  =  Map.Map CmdName KureCmdSetting


-- Basic lookups/helper functions: --------------------------------------------

relLookup     ::  CmdName -> KureCmdSettings -> Maybe Relation
relLookup      =  (rel <$>) .* Map.lookup

matcherLookup ::  CmdName -> KureCmdSettings -> Maybe Matcher
matcherLookup  =  (matcher <$>) .* Map.lookup

refinerLookup ::  CmdName -> KureCmdSettings -> Maybe Refiner
refinerLookup  =  (refiner <$>) .* Map.lookup

interpLookup  ::  CmdName -> KureCmdSettings -> Maybe Interp
interpLookup   =  (interp <$>) .* Map.lookup

allMatchers   ::  KureCmdSettings -> [Matcher]
allMatchers    =  fmap matcher . Map.elems

allCmdNames   ::  KureCmdSettings -> [CmdName]
allCmdNames    =  Map.keys

-- Actual settings: -----------------------------------------------------------

kureCmdSettings :: KureCmdSettings
kureCmdSettings  = Map.fromList

 [

 -- BasicKureCmd: -------------------------------------------------------------

  ("apply-def"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = BasicKureCmd.matcher_applyDef
                   , refiner = BasicKureCmd.refiner_applyDef
                   , interp  = BasicKureCmd.interp_applyDef
                   })
 ,
  ("unapply-def"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = BasicKureCmd.matcher_unapplyDef
                   , refiner = BasicKureCmd.refiner_unapplyDef
                   , interp  = BasicKureCmd.interp_unapplyDef
                   })
 ,
  ("rename-binder"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = BasicKureCmd.matcher_renameBinder
                   , refiner = BasicKureCmd.refiner_renameBinder
                   , interp  = BasicKureCmd.interp_renameBinder
                   })
 ,
  ("gen-new-binders"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = BasicKureCmd.matcher_genNewBinders
                   , refiner = BasicKureCmd.refiner_genNewBinders
                   , interp  = BasicKureCmd.interp_genNewBinders
                   })
 ,
  ("desugar"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = BasicKureCmd.matcher_desugar
                   , refiner = BasicKureCmd.refiner_desugar
                   , interp  = BasicKureCmd.interp_desugar
                   })
 ,
  ("tick-intro"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = BasicKureCmd.matcher_tickIntro
                   , refiner = BasicKureCmd.refiner_tickIntro
                   , interp  = BasicKureCmd.interp_tickIntro
                   })
 ,
  ("sugar"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = BasicKureCmd.matcher_sugar
                   , refiner = BasicKureCmd.refiner_sugar
                   , interp  = BasicKureCmd.interp_sugar
                   })
  ,
  ("tick-elim"
  , KureCmdSetting { rel     = R_I
                   , matcher = BasicKureCmd.matcher_tickElim
                   , refiner = BasicKureCmd.refiner_removeTick
                   , interp  = BasicKureCmd.interp_removeTick
                   })
 ,
  ("untick-intro"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = BasicKureCmd.matcher_untickIntro
                   , refiner = BasicKureCmd.refiner_removeTick
                   , interp  = BasicKureCmd.interp_removeTick
                   })

-- TickKureCmd: ---------------------------------------------------------------

 ,
  ("beta"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_beta
                   , refiner = TickKureCmd.refiner_beta
                   , interp  = TickKureCmd.interp_beta
                   })
 ,
  ("unbeta"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_unbeta
                   , refiner = TickKureCmd.refiner_unbeta
                   , interp  = TickKureCmd.interp_unbeta
                   })
 ,
  ("case-beta"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_caseBeta
                   , refiner = TickKureCmd.refiner_caseBeta
                   , interp  = TickKureCmd.interp_caseBeta
                   })
 ,
  ("uncase-beta"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_uncaseBeta
                   , refiner = TickKureCmd.refiner_uncaseBeta
                   , interp  = TickKureCmd.interp_uncaseBeta
                   })
 ,
  ("gc"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_gc
                   , refiner = TickKureCmd.refiner_gc
                   , interp  = TickKureCmd.interp_gc
                   })
 ,
  ("gc-all"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_gcAll
                   , refiner = TickKureCmd.refiner_gc
                   , interp  = TickKureCmd.interp_gc
                   })
 ,
  ("ungc"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_ungc
                   , refiner = TickKureCmd.refiner_ungc
                   , interp  = TickKureCmd.interp_ungc
                   })
 ,
  ("let-flatten"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_letFlatten
                   , refiner = TickKureCmd.refiner_letFlatten
                   , interp  = TickKureCmd.interp_letFlatten
                   })
 ,
  ("unlet-flatten"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_unletFlatten
                   , refiner = TickKureCmd.refiner_unletFlatten
                   , interp  = TickKureCmd.interp_unletFlatten
                   })
 ,
  ("value-beta"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_valueBeta
                   , refiner = TickKureCmd.refiner_valueBeta
                   , interp  = TickKureCmd.interp_valueBeta
                   })
 ,
  ("unvalue-beta"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_unvalueBeta
                   , refiner = TickKureCmd.refiner_unvalueBeta
                   , interp  = TickKureCmd.interp_unvalueBeta
                   })
 ,
  ("let-float-val"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_letFloatVal
                   , refiner = TickKureCmd.refiner_letFloatVal
                   , interp  = TickKureCmd.interp_letFloatVal
                   })
 ,
  ("unlet-float-val"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_unletFloatVal
                   , refiner = TickKureCmd.refiner_unletFloatVal
                   , interp  = TickKureCmd.interp_unletFloatVal
                   })
 ,
  ("tick-eval"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_tickEval
                   , refiner = TickKureCmd.refiner_tickEval
                   , interp  = TickKureCmd.interp_tickEval
                   })
 ,
  ("untick-eval"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_untickEval
                   , refiner = TickKureCmd.refiner_untickEval
                   , interp  = TickKureCmd.interp_untickEval
                   })
 ,
  ("let-eval"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_letEval
                   , refiner = TickKureCmd.refiner_letEval
                   , interp  = TickKureCmd.interp_letEval
                   })
 ,
  ("unlet-eval"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_unletEval
                   , refiner = TickKureCmd.refiner_unletEval
                   , interp  = TickKureCmd.interp_unletEval
                   })
 ,
  ("case-eval"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_caseEval
                   , refiner = TickKureCmd.refiner_caseEval
                   , interp  = TickKureCmd.interp_caseEval
                   })
 ,
  ("uncase-eval"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TickKureCmd.matcher_uncaseEval
                   , refiner = TickKureCmd.refiner_uncaseEval
                   , interp  = TickKureCmd.interp_uncaseEval
                   })
 ,
  ("eval-i"
  , KureCmdSetting { rel     = R_I
                   , matcher = TickKureCmd.matcher_eval_I
                   , refiner = TickKureCmd.refiner_eval_I
                   , interp  = TickKureCmd.interp_eval_I
                   })
 ,
  ("eval-wce"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = TickKureCmd.matcher_eval_WCE
                   , refiner = TickKureCmd.refiner_eval_WCE
                   , interp  = TickKureCmd.interp_eval_WCE
                   })
 ,
  ("var-beta"
  , KureCmdSetting { rel     = R_I
                   , matcher = TickKureCmd.matcher_varBeta
                   , refiner = TickKureCmd.refiner_varBeta
                   , interp  = TickKureCmd.interp_varBeta
                   })
 , 
  ("var-beta-wce"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = TickKureCmd.matcher_varBetaWCE
                   , refiner = TickKureCmd.refiner_varBeta
                   , interp  = TickKureCmd.interp_varBeta
                   })
 -- ListKureCmd: --------------------------------------------------------------

 ,
  ("append-assoc-lr-i"
  , KureCmdSetting { rel     = R_I
                   , matcher = ListKureCmd.matcher_appAssoc_LR_I
                   , refiner = ListKureCmd.refiner_appAssoc_LR
                   , interp  = ListKureCmd.interp_appAssoc_LR
                   })
 ,
  ("append-assoc-lr-wce"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = ListKureCmd.matcher_appAssoc_LR_WCE
                   , refiner = ListKureCmd.refiner_appAssoc_LR
                   , interp  = ListKureCmd.interp_appAssoc_LR
                   })
 ,
  ("append-assoc-rl-wi"
  , KureCmdSetting { rel     = R_WI
                   , matcher = ListKureCmd.matcher_appAssoc_RL_WI
                   , refiner = ListKureCmd.refiner_appAssoc_RL
                   , interp  = ListKureCmd.interp_appAssoc_RL
                   })
 ,
  ("append-assoc-rl-wce"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = ListKureCmd.matcher_appAssoc_RL_WCE
                   , refiner = ListKureCmd.refiner_appAssoc_RL
                   , interp  = ListKureCmd.interp_appAssoc_RL
                   })
 ,
  ("append-ident"
  , KureCmdSetting { rel     = R_I
                   , matcher = ListKureCmd.matcher_appIdent
                   , refiner = ListKureCmd.refiner_appIdent
                   , interp  = ListKureCmd.interp_appIdent
                   })

 -- MiscKureCmd: --------------------------------------------------------------

 ,
  ("rotate-bindings-c"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = MiscKureCmd.matcher_rotateBindingsC
                   , refiner = MiscKureCmd.refiner_rotateBindingsC
                   , interp  = MiscKureCmd.interp_rotateBindingsC
                   })
 ,
  ("rotate-bindings-cc"
  , KureCmdSetting { rel     = R_EQ
                   , matcher = MiscKureCmd.matcher_rotateBindingsCC
                   , refiner = MiscKureCmd.refiner_rotateBindingsCC
                   , interp  = MiscKureCmd.interp_rotateBindingsCC
                   })

  -- WorkerWrapperKureCmd: ----------------------------------------------------
 ,
  ("ww-ass-a"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = WorkerWrapperKureCmd.matcher_wwAssA
                   , refiner = WorkerWrapperKureCmd.refiner_wwAssA
                   , interp  = WorkerWrapperKureCmd.interp_wwAssA
                   })
 ,
  ("unww-ass-a"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = WorkerWrapperKureCmd.matcher_unwwAssA
                   , refiner = WorkerWrapperKureCmd.refiner_unwwAssA
                   , interp  = WorkerWrapperKureCmd.interp_unwwAssA
                   })
 ,
  ("ww-ass-c"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = WorkerWrapperKureCmd.matcher_wwAssC
                   , refiner = WorkerWrapperKureCmd.refiner_wwAssC
                   , interp  = WorkerWrapperKureCmd.interp_wwAssC
                   })
 ,
  ("unww-ass-c"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = WorkerWrapperKureCmd.matcher_unwwAssC
                   , refiner = WorkerWrapperKureCmd.refiner_unwwAssC
                   , interp  = WorkerWrapperKureCmd.interp_unwwAssC
                   })
 ,
  ("rolling-rule"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = WorkerWrapperKureCmd.matcher_rollingRule
                   , refiner = WorkerWrapperKureCmd.refiner_rollingRule
                   , interp  = WorkerWrapperKureCmd.interp_rollingRule
                   })
 ,
  ("unrolling-rule"
  , KureCmdSetting { rel     = R_WCE
                   , matcher = WorkerWrapperKureCmd.matcher_unrollingRule
                   , refiner = WorkerWrapperKureCmd.refiner_unrollingRule
                   , interp  = WorkerWrapperKureCmd.interp_unrollingRule
                   })

  -- TabulateKureCmd: ---------------------------------------------------------

 ,
  ("plus-right-ident"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TabulateKureCmd.matcher_plusRightIdent
                   , refiner = TabulateKureCmd.refiner_plusRightIdent
                   , interp  = TabulateKureCmd.interp_plusRightIdent
                   })
 ,
  ("plus-left-ident"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TabulateKureCmd.matcher_plusLeftIdent
                   , refiner = TabulateKureCmd.refiner_plusLeftIdent
                   , interp  = TabulateKureCmd.interp_plusLeftIdent
                   })
 ,
  ("plus-assoc"
  , KureCmdSetting { rel     = R_CE
                   , matcher = TabulateKureCmd.matcher_plusAssoc
                   , refiner = TabulateKureCmd.refiner_plusAssoc
                   , interp  = TabulateKureCmd.interp_plusAssoc
                   })

 ]

