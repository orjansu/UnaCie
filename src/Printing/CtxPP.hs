
module CtxPP (pprCon) where
import CtxAST        ( Alt(..), Bind(..), Con(..)
                     , Ctx(..), GBind(..), Name )
import CtxUtils      (bindsToPairs)
import CtxKind       (ctxKindToChar)
import PrintSettings (terminalLineStyle)
import Utils         (deggar)
import PPLib
  ( Outputable(..)
  , arr
  , dot
  , emptyHole
  , eq
  , esac
  , fo
  , ifCons
  , isInfix
  , lambda
  , ni
  , nil
  , tel
  , tick
  , toInfix
  , underscore
  , wildcard
  )

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ
  ( Doc
  , ($$)
  , (<+>)
  , (<>)
  , char
  , comma
  , fsep
  , hcat
  , int
  , lbrack
  , maybeParens
  , nest
  , parens
  , rbrack
  , render
  , renderStyle
  , sep
  , text
  , vcat
  )

{-
   Information:
   ----------------------------------------------------------------------------
   - Pretty printing for CtxAST.

   Working notes:
   ----------------------------------------------------------------------------
   - This is to be replaced by CtxShowAST in due course.
-}

instance Outputable GBind where
  ppr (CBind k ns ctx) = char (ctxKindToChar k)
                           <> underscore
                           <> text ns
                           <+> eq
                           <+> ppr ctx
  ppr (TBind ns ctx) = text ns <+> eq <+> ppr ctx

instance Outputable Ctx where
  ppr (Var ns)     = text ns
  ppr (LitInt i)   = int i
  ppr (LitStr s)   = text ('\"' : s ++ "\"")
  ppr (Abs ns ctx) = hcat [lambda, text ns, dot, ppr ctx]
  ppr (Tick ctx)   = tick <> pprParen ctx
  ppr Hole         = emptyHole
  ppr ctx@App{}    = pprApp ctx
  ppr ctx@AppD{}   = pprAppD ctx

  ppr (Let bs ctx) = sep [tel <+> vcat pprBs, ni <+> ppr ctx]
    where
      pprBs    = zipWith (\s c -> text s <+> eq <+> ppr c) (deggar ss) cs
      (ss, cs) = unzip (bindsToPairs bs)

  ppr (Case ctx as) = (esac <+> ppr ctx <+> fo) $$ nest 1 (vcat pprAs)
    where
      pprAs    = zipWith (\s c -> text s <+> arr <+> ppr c) (deggar ss) cs
      (ss, cs) = unzip (fmap pprAlt' as)

  ppr (CVar k name Nothing) =
    hcat [char (ctxKindToChar k), underscore, text name, emptyHole]

  ppr (CVar k name (Just ctx)) =
    hcat [char (ctxKindToChar k), underscore, text name, lbrack, ppr ctx, rbrack]

instance Outputable Bind where
  ppr = pprBind

instance Outputable Alt where
  ppr = pprAlt

-- Default show instance assume terminal width: --

instance Show GBind where
  show = renderStyle terminalLineStyle  . ppr

instance Show Ctx where
  show = renderStyle terminalLineStyle  . ppr

instance Show Bind where
  show = renderStyle terminalLineStyle  . ppr

instance Show Alt where
  show = renderStyle terminalLineStyle  . ppr

-- Applications: --------------------------------------------------------------

pprApp :: Ctx -> Doc
pprApp ctx = go ctx []
  where
    go (App c1 c2) cs = go c1 (c2 : cs)
    go (Var ns) cs
      | isInfix ns  = case cs of
         [c1,c2] -> pprParen c1
                     <+> toInfix ns
                     <+> pprParen c2
         (c1 : c2 : cs') -> (parens $ pprParen c1
                             <+> toInfix ns
                             <+> pprParen c2)
                             <+> (fsep . fmap pprParen $ cs')
          -- Partial application
         [c1] -> text ns <+> (pprParen c1)

         -- Just the variable (can't happen in practice).
         []   -> text ns
    go ctx cs = fsep . fmap pprParen $ (ctx : cs)

-- Data types: ----------------------------------------------------------------

pprAppD :: Ctx -> Doc
pprAppD (AppD NIL []) = nil
pprAppD (AppD CONS [Var ns1, Var ns2]) = parens (text ns1 <> ifCons <> text ns2)
pprAppD l@(AppD CONS _)
 | isUniformAtomicList l = lbrack <> (hcat
                                    . intersperse comma
                                    . fmap ppr
                                    . fromList) l <> rbrack
 | isList l = hcat . intersperse ifCons . fmap pprParen . fromList $ l
 | isMixedList l = pprParen h <> ifCons <> pprParen t
    where AppD CONS [h, t] = l
pprAppD _ = text "## unrecognised data type ##"

-- Let bindings: --------------------------------------------------------------

pprBind :: Bind -> Doc
pprBind (Bind ns ctx _) = text ns <+> eq <+> ppr ctx

-- Case alternatives: ---------------------------------------------------------

pprAlt :: Alt -> Doc
pprAlt (Alt con ns ctx _) = pprCon con ns <+> arr <+> ppr ctx

pprAlt' :: Alt -> (String, Ctx)
pprAlt' (Alt con ns ctx _) = (render $ pprCon con ns, ctx)

-- Data type constructors: ----------------------------------------------------

pprCon :: Con -> [Name] -> Doc
pprCon VARIABLE   [n] = text n
pprCon CONS       [n] = lbrack <> text n <> rbrack
pprCon CONS       ns  = (parens . hcat . intersperse ifCons . fmap text) ns
pprCon NIL        []  = nil
pprCon DEFAULT    []  = wildcard
pprCon (LITINT i) []  = int i
pprCon (LITSTR s) []  = text ('\"' : s ++ "\"")
pprCon _          _   = text "## unrecognised data constructor ##"

-- Helpers: -------------------------------------------------------------------

-- Parenthesise if /not/ printably atomic
pprParen :: Ctx -> Doc
pprParen ctx = maybeParens (not . atomic $ ctx) (ppr ctx)

-- Printably atomic.
atomic :: Ctx -> Bool
atomic Var{}    = True
atomic LitInt{} = True
atomic LitStr{} = True
atomic Tick{}   = True
atomic AppD{}   = True
atomic Hole     = True
atomic CVar{}   = True
atomic _        = False

-- Build a [Ctx] from an AppD /list/.
fromList :: Ctx -> [Ctx]
fromList (AppD NIL [])       = []
fromList (AppD CONS [c1,c2]) = c1 : fromList c2
fromList ctx                 = [ctx]

-- Checking the form of list elements: --

isUniformAtomicList :: Ctx -> Bool
isUniformAtomicList (AppD NIL [])             = True
isUniformAtomicList (AppD CONS [Var{}, t])    = isVarList t
isUniformAtomicList (AppD CONS [LitInt{}, t]) = isLitIntList t
isUniformAtomicList (AppD CONS [LitStr{}, t]) = isLitStrList t
isUniformAtomicList _                         = False

isVarList :: Ctx -> Bool
isVarList (AppD NIL [])          = True
isVarList (AppD CONS [Var{}, t]) = isVarList t
isVarList _                      = False

isLitIntList :: Ctx -> Bool
isLitIntList (AppD NIL [])             = True
isLitIntList (AppD CONS [LitInt{}, t]) = isLitIntList t
isLitIntList _                         = False

isLitStrList :: Ctx -> Bool
isLitStrList (AppD NIL [])             = True
isLitStrList (AppD CONS [LitStr{}, t]) = isLitStrList t
isLitStrList _                         = False

isMixedList :: Ctx -> Bool
isMixedList (AppD NIL [])             = True
isMixedList (AppD CONS [_, t@AppD{}]) = isMixedList t
isMixedList (AppD CONS [_, _])        = True
isMixedList _                         = False

isList :: Ctx -> Bool
isList (AppD NIL [])      = True
isList (AppD CONS [_, t]) = isList t
isList _                  = False
