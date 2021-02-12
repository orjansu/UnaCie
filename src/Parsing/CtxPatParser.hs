{-# LANGUAGE TupleSections #-}

module CtxPatParser
  ( ctx           -- Single context pattern.
  , ctx'          -- Single context pattern, must have hole.
  , ctxs          -- Sequence of context patterns.
  , ctxsAllHoles  -- Sequence of context patterns, all must have holes.
  , ctxsAnyHoles  -- Sequence of context patterns, at least one must have a hole.
  , term          -- A context pattern without a hole.
  ) where

import CtxPatAST ( AltPat(..), BindPat(..)
                 , CtxConstPat(..), CtxPat(..)
                 , bindPatBinder, ctxToCtxPat, hasHoles )
import Utils     (noDupes)
import qualified SourceParserLib as SPL
import qualified CtxParser as CP

import IndentationParserLib
  ( Parser
  , (+++)
  , bracket
  , chainl1
  , choice
  , emptyhole_
  , eq_
  , isSymChar
  , lbrack_
  , lparen_
  , manyOffside1
  , mid_
  , rbrack_
  , rparen_
  , sepby1
  , sym
  , wildcard_
  )

import Prelude hiding (abs)
import Control.Monad  (guard)
import Data.Maybe     (catMaybes)

{-
   <TO-DO>: - AppD is omitted for now.

   Information:
   ------------
   - Parser for context patterns, generates abstract syntax from CtxPatAST;
   - We're not concerned with parsing error messages for the time being;
   - Very similar to CtxParser, whereby we make heavy use of SPL's
     functionality.

   Working notes:
   --------------
   - ctxsAnyHoles used for parsing nesting patterns from the command line.
   - ctxAllHoles used for parsing cost-equivalent context patterns from files.
   - ctx' used for parsing a single cost-equivalent context pattern from the
     command line.
-}

-------------------------------------------------------------------------------
-- CtxPastAST parsers: --
-------------------------------------------------------------------------------

ctxs :: Parser [CtxPat]
ctxs  = manyOffside1 ctx +++ (ctx `sepby1` sym ",")

-- As above but at least one pattern must have holes:--

ctxsAnyHoles :: Parser [CtxPat]
ctxsAnyHoles  = ctxs >>= \cs -> cs <$ guard (any hasHoles cs)

-- As above but all patterns must have holes: --

ctxsAllHoles :: Parser [CtxPat]
ctxsAllHoles  = ctxs >>= \cs -> cs <$ guard (all hasHoles cs)

ctx :: Parser CtxPat
ctx  = (list +++ nonAppCtx) `chainl1` app

term :: Parser CtxPat
term  = ctx >>= \c -> c <$ guard (not $ hasHoles c)

-- As above but must have holes: --

ctx' :: Parser CtxPat
ctx'  = ctx >>= \c -> c <$ guard (hasHoles c)

-- Constructors: --------------------------------------------------------------

var :: Parser CtxPat
var  = SPL.var PVar

litInt :: Parser CtxPat
litInt  = SPL.litInt PLitInt

litStr :: Parser CtxPat
litStr  = SPL.litStr PLitStr

abs :: Parser CtxPat
abs  = SPL.abs PAbs ctx

tick :: Parser CtxPat
tick  = SPL.tick PTick ctx

-- Use the ctx version and convert: saves code duplication and only minor
-- conversion.
list :: Parser CtxPat
list  = ctxToCtxPat <$> CP.list

-- Here we convert infix operators to prefixs.
app :: Parser (CtxPat -> CtxPat -> CtxPat)
app  = return $ \c1 c2 -> case c2 of
         PVar vs | all isSymChar vs -> PApp (PVar $ '(' : vs ++ ")") c1
         _  -> PApp c1 c2

tel :: Parser CtxPat
tel  = do
         l@(PLet bs _) <- SPL.tel PLet bind ctx
         -- Ensure wildcard pattern appears last if at all
         guard (BindWildcard `notElem` init bs)
         -- Ensure bind names are unique
         guard (noDupes . catMaybes $ fmap bindPatBinder bs)
         return l

bind :: Parser BindPat
bind  =  SPL.bind PBind ctx
           +++ (const BindWildcard <$> (wildcard_ *> eq_ *> wildcard_))
           +++ (const BindWildcard <$> wildcard_)

esac :: Parser CtxPat
esac  = SPL.esac PCase ctx alt >>= \c@(PCase _ as) ->
         -- Ensure wildcard pattern appears last if at all.
         c <$ guard (AltWildcard `notElem` init as)

alt :: Parser AltPat
alt  = SPL.alt PAlt ctx
        +++ (const AltWildcard <$> (wildcard_ *> eq_ *> wildcard_))
        +++ (const AltWildcard <$> wildcard_)

-- Omitted for now.
-- appD :: Parser CtxPat
-- appD  = undefined

-- Holes can be substituted.
hole :: Parser CtxPat
hole  = (const (PHole Nothing) <$> emptyhole_)
         -- Explicitly substituted hole: distinguishes [x] == [-](x)
         --  from the singleton list [x]
         +++ (PHole . Just <$> bracket lbrack_ (bracket mid_ ctx mid_) rbrack_
         -- Implicitly substituted hole.
         +++ bracket lbrack_ ctx rbrack_)

-- PCVar's holes can be substituted.
cVar :: Parser CtxPat
cVar  = (uncurry . PCVar) <$> SPL.cBindName <*> (empty +++ noSub +++ sub)
        where
          empty = const (Nothing, True) <$> emptyhole_
          noSub = (, False) . Just <$> bracket lbrack_ ctx rbrack_
          -- For CVars we have to explicitly mark the holes as
          -- substituted w.r.t. /nestings/ as CVars can be substituted
          -- in terms anyway.
          sub = (, True) . Just <$> bracket lbrack_ (bracket mid_ ctx mid_) rbrack_

-- Wildcard notation.
wildcard :: Parser CtxPat
wildcard  = const Wildcard <$> wildcard_

-- Constructor patterns.
ctxConst :: Parser CtxPat
ctxConst  = choice
             [ const (ConstPat P_VAR)     <$> sym "VAR"
             , const (ConstPat P_LIT_INT) <$> sym "INT"
             , const (ConstPat P_LIT_STR) <$> sym "STR"
             , const (ConstPat P_ABS)     <$> sym "ABS"
             , const (ConstPat P_APP)     <$> sym "APP"
             , const (ConstPat P_TICK)    <$> sym "TICK"
             , const (ConstPat P_LET)     <$> sym "LET"
             , const (ConstPat P_CASE)    <$> sym "CASE"
             , const (ConstPat P_DATA)    <$> sym "DATA"
             , const (ConstPat P_LIST)    <$> sym "LIST"
             ]

bCtx :: Parser CtxPat
bCtx  = bracket lparen_ ctx rparen_

nonAppCtx :: Parser CtxPat
nonAppCtx  = choice
              [ litInt   -- PLitInt
              , litStr   -- PLitStr
              , abs      -- PAbs
              , tick     -- PTick
              , tel      -- PLet
              , esac     -- PCase
              , hole     -- PHole
              , cVar     -- PCVar
              , ctxConst -- ConstPat
              , wildcard -- Wildcard
              , var      -- PVar
              , bCtx     -- ( <ctx> )
              ]
