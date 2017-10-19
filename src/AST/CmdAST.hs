{-# LANGUAGE FlexibleInstances #-}

module CmdAST
  ( Cmd(..)              -- Command datatype.
  , CmdName              -- Command names are strings.
  , LocatedRawParam(..)  -- Located raw parameters store position info.
  , Param(..)            -- Parameter datatype.
  , RawCmd(..)           -- Raw command datatype.
  , RawParam(..)         -- Raw parameter datatype.
  , SafeRawCmd           -- Safe raw commands have been inequationally verified.
  , Script               -- Scripts = lists of raw commands.
  , UnsafeRawCmd         -- Unsafe raw commands have not (yet) been 
                         -- inequationally verified.
  ) where 

import CmdLexUtils   (Pos)
import CtxKind       (CtxKind)
import CtxPatAST     (UPat)
import CtxPatPP      ()
import Navigation    (Direction)
import PPLib         (Outputable, ppr)
import PrintSettings (terminalLineStyle)
import Relations     (Relation, relToStr)
import Universes     (U)

import Text.PrettyPrint.HughesPJ
  ( ($+$)
  , (<+>)
  , (<>)
  , char
  , empty
  , int
  , renderStyle
  , sep
  , text
  )

{-    
   Information:
   ----------------------------------------------------------------------------
   - Abstract syntax for commands;
   - Raw commands/raw parameters are parsed from command line input/scripts
     and then refined (if valid) to commands/parameters once they have 
     been checked to ensure their application is safe by CmdRelCheck (i.e.,
     UNIE's inequational check, see CmdRefine for details).

  Working notes:
  -----------------------------------------------------------------------------
  - Raw parameters have their location stored should they be invalid and 
    errors need to be reported back to the user;
  - Raw commands are only established if their corresponding name is valid,  
    so no need to store the location of the respective token in this case.
-}

type CmdName = String
type Script  = [RawCmd] 

-- Raw: -----------------------------------------------------------------------

type UnsafeRawCmd = RawCmd  -- Unsafe before inequational validation
type SafeRawCmd   = RawCmd  -- Safe after validation

data RawCmd 
  = RawNavCmd         CmdName [LocatedRawParam]   -- Navigation commands (left, right, etc.)
  | RawShellCmd       CmdName [LocatedRawParam]   -- Shell commands (exit, clear, etc.)
  | RawTransEnvCmd    CmdName [LocatedRawParam]   -- Proof commands (show-hist, back-hist, etc.)
  | RawBaseLibCmd     CmdName [LocatedRawParam]   -- Updates to the BaseLib (show-lib, add-lib, delete-lib, etc.)
  | RawScriptCmd      CmdName [LocatedRawParam]   -- Script commands (export-script, load-script, etc.)
  | RawKureCmd        CmdName [LocatedRawParam]   -- Kure transformation commands, i.e., from the std. library
  | RawStateCmd       CmdName [LocatedRawParam]   -- Commands that change the interpreter state (trans, end-trans, etc.)
  | RawAssumptionCmd  CmdName [LocatedRawParam]   -- Assumptions
    deriving Eq

-- Located raw parameters have position information (col/line number).
data LocatedRawParam = LocatedRawParam { par :: RawParam
                                       , pos :: Pos 
                                       } deriving Eq

data RawParam 
  = RawSrcName String                       -- Source names ('reverse, 'f) etc.             
  | RawSrcCode String                       -- Source code (anything $-$, might be invalid)
  | RawCmdName String                       -- Command name (any named command/parameter)
  | RawProp    RawParam RawParam RawParam   -- Proposition (e.g., t1 `improves` t2)
  | RawNumber  Int                          -- Number
  | RawFile    FilePath                     -- Filepath
    deriving Eq

-- Fully fledged: -------------------------------------------------------------
-- Usually one-to-one with raw, but we introduce some extra e.g., separating
-- term/context names.

data Cmd 
  = NavCmd        Direction                     
  | ShellCmd      CmdName [Param]
  | TransEnvCmd   CmdName [Param]
  | BaseLibCmd    CmdName [Param]
  | ScriptCmd     CmdName [Param]
  | StateCmd      CmdName [Param]
  | KureCmd       CmdName [Param]
  | AssumptionCmd CmdName [Param]
    deriving Eq

data Param 
  = CtxSrcName     String     -- Separate term/ctx names
  | TermSrcName    String 
  | CmdName        String
  | SrcCode        U          -- Separate contexts and context patterns
  | TermSrcCode    U          -- and term/context source code
  | CtxSrcCode     U 
  | PatSrcCode     UPat
  | TermPatSrcCode UPat
  | CtxPatSrcCode  UPat
  | Number         Int 
  | File           FilePath   -- FilePath may be invalid
  | Rel            Relation
  | Prop           Param Relation Param
  | CtxKind        CtxKind    -- Context kind (standard/value etc.)
    deriving Eq

-- Pretty printing: -----------------------------------------------------------
{-
  - The default show instance for commands has a 'strict' line output to ensure
    that any printed commands can be subsequently parsed as scripts (see 
    CmdLexer.x for lexing details);
  - A strict line output ensures that all commands and their parameters appear 
    on the /same/ line, with one exception: only the leading and trailing $s 
    (i.e., source code delimiters) need be on the same /line/ as previous/next 
    command parameters. Thus, source code can be laid out across multiple lines 
    to help with its clarity.
  - We re-insert parameter delimiters, e.g., ' for source names.
-}

-- None of the below code is interesting, it's just formatting: -- 

instance Outputable RawParam where 
  ppr (RawSrcName s)     = char '\'' <> text s
  ppr (RawSrcCode s)     = char '$' <> text s <> char '$'
  ppr (RawCmdName s)     = text s 
  ppr (RawFile    s)     = text s 
  ppr (RawNumber  n)     = int n
  ppr (RawProp p1 pr p2) = ppr [p1, pr, p2]

instance Outputable LocatedRawParam where 
  ppr (LocatedRawParam par pos) = sep [ppr pos, ppr par]

instance Outputable [RawParam] where 
  ppr []                        = empty 
  ppr [rp]                      = ppr rp
  ppr (RawSrcCode s : rps)      = sep [ char '$', text s
                                      , char '$' <+> ppr rps ]
  ppr (rp : RawSrcCode s : rps) = sep [ ppr rp <+> char '$', text s
                                      , char '$' <+> ppr rps ]
  ppr (rp : rps)                = ppr rp <+> ppr rps

instance Outputable [LocatedRawParam] where 
  ppr []           = empty
  ppr (lrp : lrps) = ppr lrp $+$ ppr lrps

instance Outputable RawCmd where 
  ppr (RawNavCmd        s lrps) = text s <+> ppr lrps
  ppr (RawShellCmd      s lrps) = text s <+> ppr lrps       
  ppr (RawTransEnvCmd   s lrps) = text s <+> ppr lrps
  ppr (RawBaseLibCmd    s lrps) = text s <+> ppr lrps
  ppr (RawScriptCmd     s lrps) = text s <+> ppr lrps
  ppr (RawKureCmd       s lrps) = text s <+> ppr lrps
  ppr (RawStateCmd      s lrps) = text s <+> ppr lrps
  ppr (RawAssumptionCmd s lrps) = text s <+> ppr lrps

instance Outputable Param where 
  ppr (CtxSrcName  s)       = char '\'' <> text s
  ppr (TermSrcName s)       = char '\'' <> text s
  ppr (CmdName     s)       = text s
  ppr (File        s)       = text s
  ppr (Number      n)       = int n 
  ppr (Rel         r)       = text (relToStr r)
  ppr (SrcCode     u)       = char '$' <+> ppr u <+> char '$'
  ppr (TermSrcCode u)       = char '$' <+> ppr u <+> char '$'
  ppr (CtxSrcCode  u)       = char '$' <+> ppr u <+> char '$'
  ppr (PatSrcCode     upat) = char '$' <+> ppr upat <+> char '$'
  ppr (TermPatSrcCode upat) = char '$' <+> ppr upat <+> char '$'
  ppr (CtxPatSrcCode  upat) = char '$' <+> ppr upat <+> char '$'
  ppr (Prop p1 r p2)        = ppr [p1, Rel r, p2]
  ppr (CtxKind k)           = text (show k)

-- Basically we want <param>$<newline><code><newline>$<param>
instance Outputable [Param] where 
  ppr []                             = empty
  ppr (SrcCode u               : ps) = char '$' $+$ ppr u 
                                        $+$ char '$' <+> ppr ps 
  ppr (p : SrcCode u           : ps) = ppr p <+> char '$' $+$ ppr u 
                                        $+$ char '$' <+> ppr ps
  ppr (TermSrcCode u           : ps) = char '$' $+$ ppr u 
                                        $+$ char '$' <+> ppr ps
  ppr (p : TermSrcCode u       : ps) = ppr p <+> char '$' $+$ ppr u 
                                        $+$ char '$' <+> ppr ps
  ppr (CtxSrcCode u            : ps) = char '$' $+$ ppr u 
                                        $+$ char '$' <+> ppr ps 
  ppr (p : CtxSrcCode u        : ps) = ppr p <+> char '$' $+$ ppr u 
                                        $+$ char '$' <+> ppr ps
  ppr (PatSrcCode upat         : ps) = char '$' $+$ ppr upat 
                                        $+$ char '$' <+> ppr ps 
  ppr (p : PatSrcCode upat     : ps) = ppr p <+> char '$' $+$ ppr upat 
                                        $+$ char '$' <+> ppr ps 
  ppr (TermPatSrcCode upat     : ps) = char '$' $+$ ppr upat 
                                        $+$ char '$' <+> ppr ps
  ppr (p : TermPatSrcCode upat : ps) = ppr p <+> char '$' $+$ ppr upat 
                                        $+$ char '$' <+> ppr ps
  ppr (CtxPatSrcCode upat      : ps) = char '$' $+$ ppr upat 
                                        $+$ char '$' <+> ppr ps 
  ppr (p : CtxPatSrcCode upat  : ps) = ppr p <+> char '$' $+$ ppr upat
                                        $+$ char '$' <+> ppr ps
  ppr (p : ps)                       = ppr p <+> ppr ps  

instance Outputable Cmd where 
  ppr (BaseLibCmd    s ps) = text s <+> ppr ps
  ppr (StateCmd      s ps) = text s <+> ppr ps 
  ppr (ScriptCmd     s ps) = text s <+> ppr ps
  ppr (TransEnvCmd   s ps) = text s <+> ppr ps
  ppr (KureCmd       s ps) = text s <+> ppr ps
  ppr (ShellCmd      s ps) = text s <+> ppr ps
  ppr (AssumptionCmd s ps) = text s <+> ppr ps
  ppr (NavCmd dir)         = text (show dir)

-- Show instances => assume displayed in the terminal: -- 

instance Show RawCmd where       
  show = renderStyle terminalLineStyle . ppr

instance Show LocatedRawParam where 
  show = renderStyle terminalLineStyle . ppr

instance Show RawParam where 
  show = renderStyle terminalLineStyle . ppr

instance Show Cmd where 
  show = renderStyle terminalLineStyle . ppr
 
instance Show Param where 
  show = renderStyle terminalLineStyle . ppr