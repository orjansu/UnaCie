
module CtxPatPP () where

import CtxAST        (Con(..))
import CtxPatAST     ( AltPat(..), BindPat(..), CtxConstPat(..)
                     , CtxPat(..), UPat(..),  )
import CtxPP         (pprCon)
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
  , mid
  , tel
  , tick
  , toInfix
  , wildcard
  )

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ
  ( Doc
  , ($$)
  , (<+>)
  , (<>)
  , comma
  , fsep
  , hcat
  , hsep
  , int
  , lbrack
  , maybeParens
  , nest
  , parens
  , rbrack
  , render
  , renderStyle
  , text
  , vcat
  )

{-
   Information:
   ----------------------------------------------------------------------------
   - Pretty printing for CtxPatAST.
-}

instance Outputable UPat where
  ppr (UCtxPat  c) = ppr c
  ppr (UBindPat b) = ppr b
  ppr (UAltPat  a) = ppr a

instance Outputable CtxPat where
  ppr (PVar ns)     = text ns
  ppr (PLitInt i)   = int i
  ppr (PLitStr s)   = text ('\"' : s ++ "\"")
  ppr (PAbs ns ctx) = hcat [lambda, text ns, dot, ppr ctx]
  ppr ctx@PApp{}    = pprApp ctx
  ppr ctx@PAppD{}   = pprAppD ctx
  ppr (ConstPat c)  = pprConst c
  ppr Wildcard      = wildcard
  ppr (PTick ctx)   = tick <> pprParen ctx
  ppr (PHole Nothing) = emptyHole

  ppr (PLet [BindWildcard] ctx) = hsep [tel, wildcard, ni, ppr ctx]
  ppr (PCase ctx [AltWildcard]) = hsep [esac, ppr ctx, fo, wildcard]

  ppr (PHole (Just ctx@PVar{}))     = hcat [lbrack, mid, ppr ctx, mid, rbrack]
  ppr (PHole (Just ctx))            = lbrack <> ppr ctx <> rbrack
  ppr (PCVar name Nothing _)        = text name <> emptyHole
  ppr (PCVar name (Just ctx) True)  = hcat [ text name, lbrack
                                           , mid, ppr ctx, mid, rbrack ]
  ppr (PCVar name (Just ctx) False) = hcat [text name, lbrack, ppr ctx, rbrack]

  ppr (PLet bs ctx) = (tel <+> vcat pprBs) $$ (nest 1 $ ni <+> ppr ctx)
    where
      pprBs    = zipWith (\s c -> text s <+> eq <+> ppr c) (deggar ss) cs
      (ss, cs) = unzip (fmap pprBind' bs)

  ppr (PCase ctx as) = (esac <+> ppr ctx <+> fo) $$ nest 1 (vcat pprAs)
    where
      pprAs    = zipWith (\s c -> text s <+> arr <+> ppr c) (deggar ss) cs
      (ss, cs) = unzip (fmap pprAlt' as)

instance Outputable BindPat where
  ppr = pprBind

instance Outputable AltPat where
  ppr = pprAlt

instance Outputable CtxConstPat where
  ppr = pprConst

-- Default show assumes terminal width: --

instance Show UPat where
  show = renderStyle terminalLineStyle . ppr

instance Show CtxPat where
  show = renderStyle terminalLineStyle . ppr

instance Show BindPat where
  show = renderStyle terminalLineStyle . ppr

instance Show AltPat where
  show = renderStyle terminalLineStyle . ppr

instance Show CtxConstPat where
  show = renderStyle terminalLineStyle . ppr

-- Applications: --------------------------------------------------------------

pprApp :: CtxPat -> Doc
pprApp ctx = go ctx []
  where
    go (PApp c1 c2) cs = go c1 (c2 : cs)
    go (PVar ns)    cs | isInfix ns = case cs of
      [c1,c2] -> pprParen c1
                  <+> toInfix ns
                  <+> pprParen c2
      (c1 : c2 : cs') -> (parens $ pprParen c1
                          <+> toInfix ns
                          <+> pprParen c2)
                          <+> (fsep . fmap pprParen $ cs')
      -- Partial application
      [c1] -> parens (pprParen c1 <+> toInfix ns)
      []   -> text ns
    go ctx cs = pprParen ctx <+> (fsep . fmap pprParen $ cs)

-- Data types: ----------------------------------------------------------------

pprAppD :: CtxPat -> Doc
pprAppD (PAppD NIL []) = nil
pprAppD (PAppD CONS [PVar ns1, PVar ns2]) = parens (text ns1 <> ifCons <> text ns2)
pprAppD l@(PAppD CONS _)
 | isUniformAtomicList l = lbrack <> (hcat
                                    . intersperse comma
                                    . fmap ppr
                                    . fromList) l <> rbrack
 | isList l = hcat . intersperse ifCons . fmap pprParen . fromList $ l
 | isMixedList l = pprParen h <> ifCons <> pprParen t
    where PAppD CONS [h, t] = l
pprAppD _ = text "## unrecognised data type ##"

-- Let Bindings: --------------------------------------------------------------

pprBind :: BindPat -> Doc
pprBind BindWildcard    = wildcard
pprBind (PBind ns ctx ) = text ns <+> eq <+> ppr ctx

pprBind' :: BindPat -> (String, CtxPat)
pprBind' (PBind ns ctx ) = (ns, ctx)
pprBind' BindWildcard    = ("_", Wildcard)

-- Alternatives: --------------------------------------------------------------

pprAlt :: AltPat -> Doc
pprAlt (PAlt con ns ctx) = pprCon con ns <+> arr <+> ppr ctx
pprAlt AltWildcard       = wildcard

pprAlt' :: AltPat -> (String, CtxPat)
pprAlt' (PAlt con ns ctx) = (render $ pprCon con ns, ctx)
pprAlt' AltWildcard       = ("_", Wildcard)

-- Constructor patterns: ------------------------------------------------------

pprConst :: CtxConstPat -> Doc
pprConst P_VAR     = text "VAR"
pprConst P_LIT_INT = text "INT"
pprConst P_LIT_STR = text "STR"
pprConst P_ABS     = text "ABS"
pprConst P_APP     = text "APP"
pprConst P_TICK    = text "TICK"
pprConst P_LET     = text "LET"
pprConst P_CASE    = text "CASE"
pprConst P_DATA    = text "DATA"
pprConst P_LIST    = text "LIST"

-- Helpers: -------------------------------------------------------------------

-- Parenthesise if /not/ printably atomic.
pprParen :: CtxPat -> Doc
pprParen ctx = maybeParens (not . atomic $ ctx) (ppr ctx)

-- Printably atomic.
atomic  :: CtxPat -> Bool
atomic PVar{}     = True
atomic PLitInt{}  = True
atomic PLitStr{}  = True
atomic PTick{}    = True
atomic PAppD{}    = True
atomic PHole{}    = True
atomic PCVar{}    = True
atomic ConstPat{} = True
atomic Wildcard   = True
atomic _          = False

-- Build a [CtxPat] from an PAppD /list/.
fromList :: CtxPat -> [CtxPat]
fromList (PAppD NIL [])       = []
fromList (PAppD CONS [c1,c2]) = c1 : fromList c2
fromList ctx                  = [ctx]

-- Checking the form of list elements: --

isUniformAtomicList :: CtxPat -> Bool
isUniformAtomicList (PAppD NIL [])              = True
isUniformAtomicList (PAppD CONS [PVar{}, t])    = isVarList t
isUniformAtomicList (PAppD CONS [PLitInt{}, t]) = isLitIntList t
isUniformAtomicList (PAppD CONS [PLitStr{}, t]) = isLitStrList t
isUniformAtomicList _                           = False

isVarList :: CtxPat -> Bool
isVarList (PAppD NIL [])           = True
isVarList (PAppD CONS [PVar{}, t]) = isVarList t
isVarList _                        = False

isLitIntList :: CtxPat -> Bool
isLitIntList (PAppD NIL [])             = True
isLitIntList (PAppD CONS [PLitInt{}, t]) = isLitIntList t
isLitIntList _                         = False

isLitStrList :: CtxPat -> Bool
isLitStrList (PAppD NIL [])              = True
isLitStrList (PAppD CONS [PLitStr{}, t]) = isLitStrList t
isLitStrList _                           = False

isMixedList :: CtxPat -> Bool
isMixedList (PAppD NIL [])              = True
isMixedList (PAppD CONS [_, t@PAppD{}]) = isMixedList t
isMixedList (PAppD CONS [_, _])         = True
isMixedList _                           = False

isList :: CtxPat -> Bool
isList (PAppD NIL [])      = True
isList (PAppD CONS [_, t]) = isList t
isList _                   = False
