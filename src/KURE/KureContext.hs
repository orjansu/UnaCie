{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KureContext(KureContext(..), emptyKureContext) where

import Classes  (AddBinders(..))
import Crumb    (Crumb)
import CtxAST   (Name)

import Data.List     (nub)
import Language.KURE ( AbsolutePath, ExtendPath(..)
                     , LocalPath, ReadPath(..) )
import Language.KURE.ExtendableContext (ExtendContext(..), extendContext)

{-
  Information:
  -----------------------------------------------------------------------------
  - KURE's context stores the bound variables in scope and the current path
    through the AST;
  - Transformations have access to KURE's context whilst executing, meaning
    they can make use of the information to dictate their course of action,
    e.g., fail if variable capture will occur;
  - This saves them having to traverse the AST in order to get this
    information, see any paper on KURE for more info.

  Working notes:
  -----------------------------------------------------------------------------
  - ExtendContext to support KURE's pathfinder features;
  - See KURE's pathfinder module for more information.
-}

-- Kure context/context environment: ------------------------------------------

data KureCEnv    = KureCEnv { binders :: [Name] }
data KureContext = KureContext (AbsolutePath Crumb) KureCEnv

-- Initial settings: ----------------------------------------------------------

emptyKureEnv :: KureCEnv
emptyKureEnv  = KureCEnv { binders = mempty }

emptyKureContext :: KureContext
emptyKureContext  = KureContext mempty emptyKureEnv

-- AddBinders used to interact with bound variables in scope: -----------------
-- E.g., to check for variable capture/name clashes for safe substitution

instance AddBinders KureContext where
  addBinders bss (KureContext p env)  = KureContext p env { binders = nub
                                         (binders env ++ bss) }
  getBinders (KureContext _ env)      = binders env
  modifyBinders f (KureContext p env) = KureContext p env { binders = f
                                         (binders env) }

instance AddBinders (ExtendContext KureContext (LocalPath Crumb)) where
  addBinders bss ec  = extendContext (extraContext ec)
                        (addBinders bss $ baseContext ec)
  getBinders         = getBinders . baseContext
  modifyBinders f ec = extendContext (extraContext ec)
                        (modifyBinders f $ baseContext ec)

-- Adding crumbs to the path/read the path stored in the context: -------------

instance ExtendPath KureContext Crumb where
  (KureContext p env) @@ cr = KureContext (p @@ cr) env

instance ReadPath KureContext Crumb where
  absPath (KureContext p _) = p

instance ReadPath (ExtendContext KureContext (LocalPath Crumb)) Crumb where
  absPath = absPath . baseContext
