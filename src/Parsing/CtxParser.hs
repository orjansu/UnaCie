{-# LANGUAGE MultiWayIf #-}

module CtxParser
  ( 
    -- Various parsers of the source language: -- 

    alt'     -- Default 0 index.
  , bind'    -- Default 0 index.
  , binds    -- List of let binds, semicolon sepped.
  , cBind    -- Might not have holes.
  , cBind'   -- Must have holes.
  , ctx      -- Might not have holes.
  , ctx'     -- Must have holes.
  , cVar
  , esac
  , gBinds   -- CBinds might not have holes.
  , gBinds'  -- CBinds must have holes.
  , list     -- List sugars.
  , tBind
  , term
  , litStr

  ) where 

import CtxAST   ( Alt(..), Bind(..), Con(..)
                , Ctx(..), GBind(..), Name, Term )
import CtxKind  (CtxKind(..))
import CtxUtils (isTerm, bindBinder, hasHoles, reindexBinds)
import Utils    (noDupes)
import qualified SourceParserLib as SPL

import IndentationParserLib
  ( Parser
  , (+++)
  , bracket
  , chainl1
  , choice
  , comma_
  , dots_
  , emptyhole_
  , eq_
  , ifCons_
  , isSymChar
  , lbrack_
  , lparen_
  , manyOffside1
  , natural
  , pfCons_
  , rbrack_
  , rparen_
  , semicolon_
  , sepby0
  , sepby1
  , underscore_
  , upper
  )

import Prelude hiding (abs)
import Control.Monad  (guard, mzero)

{-
   <TO-DO>: - Can we parse [a,b,c,d] for a..d variables?
            - SPL handles duplicates for alt binders but not for let binders, 
              can we change this to make it the same for both?
            - AppD is omitted for now.
            
   Information:
   ------------
   - Parser for source language, generates abstract syntax from CtxAST;
   - We're not concerned with parsing error messages for the time being.
   - Note the 'heavy lifting' is done by the SourceParserLib, so here we just
     make use of its functionality in order to construct GBinds/Ctxs etc.

   Working notes:
   --------------
   - We don't check context /kinds/ when parsing due to the introduction
     of cost-equivalent contexts which would overcomplicate this. The check
     is now deferred to a later stage of the input.
-}

-------------------------------------------------------------------------------
-- CtxAST parsers: --
-------------------------------------------------------------------------------

-- Global bindings: -- 

gBinds :: Parser [GBind]
gBinds  = manyOffside1 (cBind +++ tBind)

-- As above but cBinds must have holes: -- 

gBinds' :: Parser [GBind]
gBinds'  = manyOffside1 (cBind' +++ tBind)

cBind :: Parser GBind
cBind  = uncurry CBind <$> ctxKindName <*> (eq_ *> ctx)

-- As above but must have holes: --

cBind' :: Parser GBind
cBind'  = uncurry CBind <$> ctxKindName <*> (eq_ *> ctx')

tBind :: Parser GBind 
tBind  = TBind <$> SPL.tBindName <*> (eq_ *> term)

-- Contexts/terms: -- 

--ctx :: Parser Ctx
--ctx  = (list +++ nonAppCtx) `chainl1` app

term :: Parser Term 
term  = ctx >>= \c -> c <$ guard (isTerm c)

-- As above but must have holes: --

ctx' :: Parser Ctx 
ctx'  = ctx >>= \c -> c <$ guard (hasHoles c)

-- Constructors: --------------------------------------------------------------

var :: Parser Ctx
var  = SPL.var Var

litInt :: Parser Ctx
litInt  = SPL.litInt LitInt 

litStr :: Parser Ctx
litStr  = SPL.litStr LitStr

abs :: Parser Ctx
abs  = SPL.abs Abs ctx

-- Here we convert infix operators to prefixs.
app :: Parser (Ctx -> Ctx -> Ctx)
app  = return $ \c1 c2 -> case c2 of 
         Var vs | all isSymChar vs -> App (Var $ '(' : vs ++ ")") c1
         _  -> App c1 c2

tick :: Parser Ctx 
tick  = SPL.tick Tick ctx 

tel :: Parser Ctx 
tel = SPL.tel (Let . fmap (uncurry ($)) . flip zip [0..]) bind ctx 
        >>= \l@(Let bs _) -> 
        -- Ensure binding names are /unique/.
        l <$ guard (noDupes $ fmap bindBinder bs)

bind :: Parser (Int -> Bind) 
bind  = SPL.bind Bind ctx 

bind' :: Parser Bind 
bind'  = SPL.bind (\ns ctx -> Bind ns ctx 0) ctx

binds :: Parser [Bind]
binds  = reindexBinds <$> bind' `sepby1` semicolon_

esac :: Parser Ctx 
esac  = SPL.esac (flip $ flip Case 
         . fmap (uncurry ($)) 
         . flip zip [0..]) ctx alt
 
alt :: Parser (Int -> Alt) 
alt  = SPL.alt Alt ctx
 
alt' :: Parser Alt 
alt'  = SPL.alt (\con ns ctx -> Alt con ns ctx 0) ctx

-- Omitted for now.
-- appD :: Parser Ctx 
-- appD  = undefined

-- Various list sugars.
list :: Parser Ctx
list  = choice 
         [ sugar1    -- [LitInt]
         , sugar2    -- [LitInt] with range
         , sugar3    -- [LitStr]
         , sugar4    -- postfix (:)
         , sugar5    -- infix   (:)
         , sugar6    -- singleton
         ]
 where 
   -- [LitInt]
   sugar1 = toList <$> bracket lbrack_ (litInt `sepby0` comma_) rbrack_  
   -- [st..fi] for [LitInt]
   sugar2 = toList <$> bracket lbrack_ ((\st fi -> fmap LitInt [st..fi]) 
                                         <$> natural
                                         <*> (dots_ *> natural)) 
                               rbrack_
   -- [LitStr]
   sugar3 = toList <$> bracket lbrack_ (litStr `sepby0` comma_) rbrack_
   -- (:) ac1 ac2, where ac1 and ac2 are atomic contexts
   sugar4 = AppD CONS <$> sequence [pfCons_ *> aCtx, aCtx]
   -- ac1 : ac2 : .. : acn where ac1, ac2, .., acn are atomic contexts
   sugar5 = AppD CONS <$> sequence [aCtx <* ifCons_, aCtx]
   sugar6 = (\actx -> AppD CONS [actx, AppD NIL []]) <$> bracket lbrack_ aCtx rbrack_

hole :: Parser Ctx
hole  = const Hole <$> emptyhole_

ctxKindName :: Parser (CtxKind, Name)
ctxKindName  = (,) <$> (upper >>= toKind) <*> (underscore_ *> SPL.cBindName)
  where  
       -- Standard contexts are ranged over by C/D (not S)
       toKind 'C' = return STD 
       toKind 'V' = return VAL
       toKind 'E' = return EVAL
       toKind 'A' = return APP
       toKind _   = mzero

cVar :: Parser Ctx
cVar  = uncurry CVar
          <$> ctxKindName 
          -- Not substituted.
          <*> (const Nothing <$> emptyhole_) 
         -- Substituted.
          +++ (Just <$> bracket lbrack_ ctx rbrack_) 
                                                    
-- Atomic context
aCtx :: Parser Ctx 
aCtx  = choice 
         [ var                               -- Variable
         , cVar                              -- Context variable
         , litInt                            -- Integer literal
         , litStr                            -- String literal
         , hole                              -- Hole
      -- , dcon                              -- Data constructor
         , bracket lparen_ ctx rparen_       -- Parenthesised context
         ]

-- Context
ctx :: Parser Ctx
ctx  = choice 
        [ (list +++ aCtx) `chainl1` app    -- Application
        , aCtx                             -- Atomic context
        , abs                              -- Abstraction
        , tel                              -- Let statement
        , esac                             -- Case statement
        , tick                             -- Tick 
        ]

-------------------------------------------------------------------------------
-- Helpers: --
-------------------------------------------------------------------------------

-- Build an AppD list from a [Ctx].
toList :: [Ctx] -> Ctx 
toList []       = AppD NIL  []
toList (c : cs) = AppD CONS [c, toList cs]