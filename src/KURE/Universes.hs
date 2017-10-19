{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Universes
  ( U(..)     -- Universe type U, see KURE paper for details.
  , L_U(..)   -- Labelled universe type.
  ) where 

import CtxAST        ( Alt, Bind, Ctx, GBind
                     , L_Alt, L_Bind, L_Ctx, L_GBind )
import CtxPP         ()
import PPLib         (Outputable(..))
import PrintSettings (terminalLineStyle)

import Language.KURE.Injection   (Injection(..))
import Text.PrettyPrint.HughesPJ (renderStyle)

{-
  Information:
  ----------------------------------------------------------------------------- 
  - The Universe type U encompasses all traversable constructors of CtxAST;
  - See KURE papers for more information as to why this is necessary.
-}

data U = UGBind GBind
       | UCtx   Ctx 
       | UBind  Bind
       | UAlt   Alt
         deriving Eq         

-- Printing: ------------------------------------------------------------------

instance Outputable U where 
  ppr (UCtx ctx)     = ppr ctx 
  ppr (UGBind gbind) = ppr gbind 
  ppr (UBind b)      = ppr b 
  ppr (UAlt a)       = ppr a

instance Show U where 
  show = renderStyle terminalLineStyle . ppr 

-- Injecting/projecting data types into/out of U: -----------------------------

instance Injection GBind U where 
  inject               = UGBind 
  project (UGBind g)   = Just g 
  project _            = Nothing

instance Injection Ctx U where 
  inject               = UCtx
  project (UCtx ctx)   = Just ctx
  project _            = Nothing

instance Injection Bind U where 
  inject               = UBind 
  project (UBind bind) = Just bind
  project _            = Nothing 

instance Injection Alt U where 
  inject               = UAlt 
  project (UAlt alt)   = Just alt 
  project _            = Nothing

-------------------------------------------------------------------------------
-- Labelling: --
-------------------------------------------------------------------------------

-- We extend U with labels in the obvious way;
-- See CtxAST for details on labelled datatypes.
data L_U a = L_UGBind (L_GBind a)
           | L_UCtx   (L_Ctx a)
           | L_UBind  (L_Bind a)
           | L_UAlt   (L_Alt a)
             deriving Eq