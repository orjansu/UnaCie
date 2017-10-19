
module InterEnv 
 ( BaseLib(..)    -- Base library of terms/contexts/cost-equiv. contexts.
 , InterEnv(..)   -- UNIE's interactive environment.
 , ScriptEnv(..)  -- Script environment: active/loaded scripts.
 , TransEnv(..)   -- Transformation environment: transformation history etc.

 -- Initial settings: --
 
 , emptyBaseLib  
 , initInterEnv
 , initScriptEnv
 , initTransEnv

 ) where 

import CmdAST      (Cmd, Script)
import Crumb       (Crumb)
import CtxAST      (Ctx, Name, Term)
import CtxEqLib    (CtxEqLib, emptyCtxEqLib)
import CtxKind     (CtxKind)
import InterState  (InterState(..))
import NavSettings (NavSettings, initNavSettings)
import Relations   (Relation)
import TransHist   (TransHist, initTransHist)
import Utils       (names)

import qualified Data.Map as Map
import Language.KURE (AbsolutePath)

{-
  Information:
  -----------------------------------------------------------------------------
  - UNIE's 'interactive environment', comprising:
  -- Base library of terms, contexts, cost-equivalent contexts;
  -- Transformation environment detailing the term under transformation, 
     current path, global relation, relation of last executed command, 
     transformation history etc.
  -- Script environment detailing loaded and active scripts. Note 
     that multiple scripts can be active at the same time, which allows 
     scripts to make use of other scripts.
  -- Interpreter state, which dictates how the interpreter responds to 
     commands etc. For example, navigation commands are only valid when 
     in the TRANS/TRANS_SCRIPT states.
-}

-- Library: terms/contexts/cost-equiv. contexts.
data BaseLib = 
  BaseLib { cBinds    :: Map.Map Name (CtxKind, Ctx)
          , tBinds    :: Map.Map Name Term          
          , ctxEqLib  :: CtxEqLib               
          }                

-- Transformation environment: term being transformed, focus path into term, 
-- global relation, transformation goal, etc.
data TransEnv = 
  TransEnv { term        :: Term
           , path        :: AbsolutePath Crumb
           , svs         :: [Name]
           , cRel        :: Maybe Relation
           , gRel        :: Maybe Relation
           , goal        :: Maybe Term
           , hist        :: TransHist
           , initTerm    :: Term
           , initBaseLib :: BaseLib
           , navSettings :: NavSettings
           }
-- Script environment which scripts are loaded/active.
data ScriptEnv = 
  ScriptEnv { scripts       :: Map.Map String Script
            , activeScripts :: [(Script, String, Int)] }

-- InterEnv comprises the above.
data InterEnv =  
  InterEnv { baseLib   :: BaseLib
           , state     :: InterState
           , mTransEnv :: Maybe TransEnv 
           , scriptEnv :: ScriptEnv 
           }

-- Initial settings: ----------------------------------------------------------

emptyBaseLib :: BaseLib
emptyBaseLib  = 
  BaseLib { cBinds   = Map.empty
          , tBinds   = Map.empty 
          , ctxEqLib = emptyCtxEqLib }

-- Transformation environment has to be initialised.
initTransEnv :: BaseLib 
                -> Term
                -> Maybe Relation 
                -> Maybe Term
                -> (Cmd, Maybe Relation) 
                -> TransEnv
initTransEnv lib t grel goal cmr  
 =  TransEnv { term        = t 
             , path        = mempty
             , svs         = names 
             , cRel        = Nothing 
             , gRel        = grel
             , goal        = goal 
             , hist        = uncurry (initTransHist t) cmr
             , initTerm    = t 
             , initBaseLib = lib
             , navSettings = initNavSettings
             }                          

initScriptEnv :: ScriptEnv
initScriptEnv  = 
  ScriptEnv { scripts       = Map.empty
            , activeScripts = [] 
             }

initInterEnv :: InterEnv
initInterEnv  =  
  InterEnv { baseLib   = emptyBaseLib
           , mTransEnv = Nothing 
           , scriptEnv = initScriptEnv
           , state     = INITIAL
           }