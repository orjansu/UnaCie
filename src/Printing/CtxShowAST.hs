{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module CtxShowAST where

import CtxAST       ( L_GBind(..), L_Ctx(..), L_Bind(..), L_Alt(..)
                    , Name, Con(..), lCtxLabel
                    , lCtxLabelCombine, lCtxLabelCombine' )
import CtxKind      (ctxKindToChar)
import Utils        (deggar)
import Universes    (L_U(..))
import PPLib hiding (toInfix) -- too many

import Data.List (intersperse, tails)
import Text.PrettyPrint.HughesPJ -- too many
import qualified Data.Map as Map

{-
  <TO-DO>: - Update printing of datatype constructors (hardcoded widths)
           - Add a pretty printing option for LaTeX
           - Add an ANSI-compatible shell check


  Information:
  -----------------------------------------------------------------------------
  - A mechanism for pretty printing ASTs by first stylising each node in the
    tree;
  - We make heavy use of ANSI escape codes, meaning UNIE must be run in an
    ANSI-compatible shell.

  Working notes:
  -----------------------------------------------------------------------------
  N/A
-}

-------------------------------------------------------------------------------
-- Show settings: --
-------------------------------------------------------------------------------
{-
  - Show settings relate to different typographical emphasis;
  - They are used in order to highlight sub-term(s) in an AST when e.g.,
    pretty printing it to the terminal;
  - In practice, we want to mark our current location inside an AST and we can
    do that by emphasising the sub-term at the location in a specific way e.g.,
    by highlighting/underlining it;
  - By the use of labelled contexts (LCtxs in CtxAST.hs), we can pair show
    settings with each constructor of an AST, and thus apply a unique style
    to each node /independently/;
  - However, in order to lessen the burden of converting a regular Ctx AST to a
    L_Ctx ShowSettings AST for the sake of pretty printing, we have introduced
    the notion of combination, which allows the parent node to combine their
    show settings with their child nodes' settings. We make use of this by
    allowing child nodes to inherit their parent's settings, meaning we only
    need to stylise the parent node at our current location, and the pretty
    printing functions can pass on this emphasis down to each child node in the
    entire sub-tree on our behalf.
    - Of course, combination doesn't have to be inheritance, and we have
      abstracted out the combining function for future experimentation.
-}
data ShowSettings = ShowSettings { highlight :: Bool
                                 , bold      :: Bool
                                 , underline :: Bool
                                 , colour    :: Colour
                                 } deriving Eq

-- Various kinds of show settings, we mainly make use of highlight: --

defaultShowSettings :: ShowSettings
defaultShowSettings  = ShowSettings { highlight = False
                                    , bold      = False
                                    , underline = False
                                    , colour    = "" }

highlightShowSettings :: ShowSettings
highlightShowSettings  = ShowSettings { highlight = True
                                      , bold      = False
                                      , underline = False
                                      , colour    = "" }

boldShowSettings :: ShowSettings
boldShowSettings  = ShowSettings { highlight = False
                                 , bold      = True
                                 , underline = False
                                 , colour    = "" }

underlineShowSettings :: ShowSettings
underlineShowSettings  = ShowSettings { highlight = False
                                      , bold      = False
                                      , underline = True
                                      , colour    = "" }

colourShowSettings :: String -> ShowSettings
colourShowSettings s = ShowSettings { highlight = False
                                    , bold      = False
                                    , underline = False
                                    , colour    = colourLookup s }

-- Show settings are applied using ANSI escape codes, meaning we can only make
-- use of them in the terminal for the time being.
instance Outputable ShowSettings where
  ppr ss = zeroWidthText $ h (highlight ss)
                        ++ b (bold      ss)
                        ++ u (underline ss)
                        ++ colour ss
           where
            h True  = "\ESC[30m\ESC[48;5;252m"
            h False = ""
            b True  = "\ESC[1m"
            b False = ""
            u True  = "\ESC[4m"
            u False = ""

escapeEnd :: Doc
escapeEnd  = zeroWidthText "\ESC[m"

-- We can apply show settings to any Doc by wrapping the ANSI escape codes
-- around it
instance Outputable (Doc, ShowSettings) where
  ppr (doc, ss) = ppr ss <> doc <> escapeEnd

-------------------------------------------------------------------------------
-- Keywords: --
-------------------------------------------------------------------------------
{-
  - We use keyword maps to store keywords (e.g., let/case etc.) with different
    emphasis/colouring;
  - These maps are then passed as arguments to the pretty printing functions;
  - This makes sense because e.g., we want to colour keywords in the terminal,
    but we don't want to colour keywords (via ANSI escape codes) that are
    printed to files.
-}

type KeywordMap = Map.Map String Doc

-- Keywords that are not stylised and are the default for file output
plainKeywords :: KeywordMap
plainKeywords  = Map.fromList
 [ ("arrow"      , arr)
 , ("case"       , esac)
 , ("colon"      , colon)
 , ("colons"     , colons)
 , ("comma"      , comma)
 , ("dot"        , dot)
 , ("eq"         , eq)
 , ("hole"       , emptyHole)
 , ("in"         , ni)
 , ("lambda"     , lambda)
 , ("lbrack"     , lbrack)
 , ("let"        , tel)
 , ("lparen"     , lparen)
 , ("nil"        , nil)
 , ("of"         , fo)
 , ("rbrack"     , rbrack)
 , ("rparen"     , rparen)
 , ("tick"       , tick)
 , ("underscore" , underscore)
 , ("wildcard"   , underscore)
 ]

-- Keywords that are stylised and are the default for terminal output
fancyKeywords :: KeywordMap
fancyKeywords  = Map.fromList
 [ ("arrow"      , arr)
 , ("case"       , ppr (esac, boldShowSettings))
 , ("colon"      , colon)
 , ("colons"     , colons)
 , ("comma"      , comma)
 , ("dot"        , dot)
 , ("eq"         , eq)
 , ("hole"       , emptyHole)
 , ("in"         , ppr (ni, boldShowSettings))
 , ("lambda"     , lambda)
 , ("lbrack"     , lbrack)
 , ("let"        , ppr (tel, boldShowSettings))
 , ("lparen"     , lparen)
 , ("nil"        , nil)
 , ("of"         , ppr (fo, boldShowSettings))
 , ("rbrack"     , rbrack)
 , ("rparen"     , rparen)
 , ("tick"       , ppr (tick, colourShowSettings "red"))
 , ("underscore" , underscore)
 , ("wildcard"   , underscore)
 ]

-- If we lookup a keyword that isn't in the given map, we print an error
-- placeholder. This saves handling Just's all over the place
pprKeyword :: String -> KeywordMap -> Doc
pprKeyword  = Map.findWithDefault miss

-- As above, but applies show settings too
pprKeyword' :: String -> KeywordMap -> ShowSettings -> Doc
pprKeyword' s map ss = ppr (Map.findWithDefault miss s map, ss)

-------------------------------------------------------------------------------
-- Outputting ShowSetting-labelled ASTs: --
-------------------------------------------------------------------------------
{-
  - We used labelled ASTs in CtxAST.hs to store show settings for every
    constructor;
  - This gives us complete control over how each component of the AST is
    displayed.
-}

-- Default way of combining styles is just to OR them
inherit :: ShowSettings -> ShowSettings -> ShowSettings
inherit ss1 ss2 = ShowSettings { highlight = highlight ss1 || highlight ss2
                               , bold      = bold ss1      || bold ss2
                               , underline = underline ss1 || underline ss2
                               , colour    = colour ss1
                               }

-- Outputting Us: -------------------------------------------------------------

-- If no keyword map/combine function is specific, we use the plain keyword map
-- and assume stylising is inherited
instance Outputable (L_U ShowSettings) where
  ppr x = ppr (x, plainKeywords, inherit)

-- If no combine function is specified, we assume stylising is inherited
instance Outputable (L_U ShowSettings, KeywordMap) where
  ppr (x, map) = ppr (x, map, inherit)

-- The most general function for pretty printing L_Ctx ShowSettings ASTs
-- Requires a keyword map and a function for combining show settings
instance Outputable ( L_U ShowSettings
                    , KeywordMap
                    , ShowSettings -> ShowSettings -> ShowSettings ) where
  ppr (L_UGBind sgbind, map, f) = ppr (sgbind, map, f)
  ppr (L_UCtx   sctx  , map, f) = ppr (sctx  , map, f)
  ppr (L_UBind  sbind , map, f) = ppr (sbind , map, f)
  ppr (L_UAlt   salt  , map, f) = ppr (salt  , map, f)

-- Outputting GBinds: ---------------------------------------------------------


instance Outputable (L_GBind ShowSettings) where
  ppr x = ppr (x, plainKeywords, inherit)

instance Outputable (L_GBind ShowSettings, KeywordMap) where
  ppr (x, map) = ppr (x, map, inherit)

instance Outputable ( L_GBind ShowSettings
                    , KeywordMap
                    , ShowSettings -> ShowSettings -> ShowSettings ) where
  ppr (L_CBind (k, ns, sctx, ss), map, f)
   = ppr (char $ ctxKindToChar k, ss)
      <> pprKeyword' "underscore" map ss
      <> ppr (text ns, ss)
      <> ppr (space, ss)
      <> pprKeyword' "eq" map ss
      <> ppr (space, ss)
      <> ppr (lCtxLabelCombine f ss sctx, map, f)
  ppr (L_TBind (ns, sctx, ss), map, f)
   = ppr (text ns, ss)
      <> ppr (space, ss)
      <> pprKeyword' "eq" map ss
      <> ppr (space, ss)
      <> ppr (lCtxLabelCombine f ss sctx, map, f)

-- Outputting Ctxs: -----------------------------------------------------------

instance Outputable (L_Ctx ShowSettings) where
 ppr x = ppr (x, plainKeywords, inherit)

instance Outputable (L_Ctx ShowSettings, KeywordMap) where
 ppr (x, map) = ppr (x, map, inherit)

instance Outputable ( L_Ctx ShowSettings
                    , KeywordMap
                    , ShowSettings -> ShowSettings -> ShowSettings ) where
  ppr (L_Var (ns, ss)  , _, _) = ppr (text ns, ss)
  ppr (L_LitInt (i, ss), _, _) = ppr (int i,   ss)
  ppr (L_LitStr (s, ss), _, _) = ppr (text $ '\"' : s ++ "\"",  ss)
  ppr x@(L_App{},        _, _) = pprApp x
  ppr (L_Hole ss,      map, _) = pprKeyword' "hole" map ss
  ppr x@(L_AppD{},       _, _) = pprAppD x

  ppr (L_Abs (ns, sctx, ss), map, f)
   = pprKeyword' "lambda" map ss
      <> ppr (text ns, ss)
      <> pprKeyword' "dot" map ss
      <> ppr (lCtxLabelCombine f ss sctx, map, f)

  ppr (L_Tick (sctx, ss), map, f)
   = pprKeyword' "tick" map ss
      <> pprParen (lCtxLabelCombine f ss sctx, map, f)

  ppr (L_Let (sbs, sctx, ss), map, f)
   = pprKeyword' "let" map ss
      <> ppr (space, ss)
      <> vcat pprBs
      $$ (nest 0 $ pprKeyword' "in" map ss
                    <> ppr (space, ss)
                    <> ppr (lCtxLabelCombine f ss sctx, map, f))
     where
      pprBs = zipWith3 (\ns sctx ss' ->
               let ss'' = f ss ss'
               in ppr (text ns, ss'')
                   <> ppr (space, ss'')
                   <> pprKeyword' "eq" map ss''
                   <> ppr (space, ss'')
                   <> ppr (lCtxLabelCombine f ss'' sctx, map, f))
                      (deggar nss) scs sss
      (nss, scs, sss) = unzip3 (fmap (\(L_Bind (ns, sctx, _, ss)) ->
                         (ns, sctx, ss)) sbs)

  ppr (L_Case (sctx, sas, ss), map, f)
   = pprKeyword' "case" map ss
      <> ppr (space, ss)
      <> ppr (lCtxLabelCombine f ss sctx, map, f)
      <> ppr (space, ss)
      <> pprKeyword' "of" map ss
      $$ (nest 1 $ vcat pprAs)
     where
      pprAs = zipWith3 (\s sctx ss' ->
               let ss'' = f ss ss'
               in s
                   <> ppr (space, ss'')
                   <> pprKeyword' "arrow" map ss''
                   <> ppr (space, ss'')
                   <> ppr (lCtxLabelCombine f ss'' sctx, map, f))
                      strs scs sss
      (strs, scs, sss) = unzip3 $ fmap (\(L_Alt (con, nss, sctx, idx, ss')) ->
                          ( pprCon con nss map (f ss' ss) (spacers !! idx)
                          , sctx
                          , ss' )) sas
      lengths = fmap (\(L_Alt (con, nss, _, _, _)) -> conWidth con nss) sas
      spacers = fmap (\i -> maxLen - i) lengths
                where maxLen = maximum lengths

  ppr (L_CVar (k, ns, Nothing, ss), map, _)
   = ppr (char (ctxKindToChar k), ss)
      <> pprKeyword' "underscore" map ss
      <> ppr (text ns, ss)
      <> pprKeyword' "hole" map ss

  ppr (L_CVar (k, ns, (Just sctx), ss), map, f)
   = ppr (char (ctxKindToChar k), ss)
      <> pprKeyword' "underscore" map ss
      <> ppr (text ns, ss)
      <> pprKeyword' "lbrack" map ss
      <> ppr (lCtxLabelCombine f ss sctx, map, f)
      <> pprKeyword' "rbrack" map ss

-- Outputting Apps: -----------------------------------------------------------

-- Complexity comes from displaying binary infix operators
pprApp :: ( L_Ctx ShowSettings
          , KeywordMap
          , ShowSettings -> ShowSettings -> ShowSettings )
          -> Doc
pprApp (sctx, map, f) = go sctx [] []
 where
  go (L_App (sc1, sc2, ss)) scs sss = go sc1 (sc2 : scs) (ss : sss)
  go sctx@(L_Var (ns, ss))  scs sss | isInfix ns = case (scs, sss) of

   -- One argument (partial application)
   ([cs1], [ss']) ->
     ppr (lCtxLabelCombine f ss' sctx, map, f)
      <> ppr (space, ss')
      <> pprParen (lCtxLabelCombine f ss' cs1, map, f)

   -- Two arguments
   ([sc1, sc2], [ss1, ss2]) ->
     pprParen (lCtxLabelCombine' f [ss1, ss2] sc1, map, f)                       -- First argument with both styles
      <> ppr (space, f ss1 ss2)                                                  -- Space with both styles
      <> ppr (lCtxLabelCombine' f [ss1, ss2] $ L_Var (toInfix ns, ss), map, f)   -- Infix operator with both styles
      <> ppr (space, ss2)                                                        -- Space with second style
      <> pprParen (lCtxLabelCombine f ss2 sc2, map, f)                           -- Second argument with second style

   -- More than two arguments
   (  (sc1 : sc2 : scs')
    , (ss1 : ss2 : sss') ) ->
     pprKeyword' "lparen" map allStyles                                          -- Outer lparen for infix app. with all styles
      <> pprParen (lCtxLabelCombine f allStyles sc1, map, f)                     -- First argument with all styles
      <> ppr (space, allStyles)                                                  -- Space with all styles
      <> ppr (lCtxLabelCombine f allStyles $ L_Var (toInfix ns, ss), map, f)     -- Infix operator with all styles
      <> ppr (space, innerStyles)                                                -- Space with inner styles
      <> pprParen (lCtxLabelCombine f innerStyles sc2, map, f)                   -- Second argument with inner styles
      <> pprKeyword' "rparen" map innerStyles                                    -- Outer rparen for infix app. with inner styles
      <> mapStyles scs' sss'
     where
      allStyles   = foldr1 f (ss1 : ss2 : sss')
      innerStyles = foldr1 f (ss2 : sss')

   -- Just the variable (can't happen in practice).
   _ -> ppr (lCtxLabelCombine' f sss sctx, map, f)

  go sctx scs sss = pprParen (lCtxLabelCombine' f sss sctx, map, f)
                     <> mapStyles scs sss

  -- Apply a list of styles over a list of application functions/arguments
  -- Not all styles are applied to every argument, so we use tails to distribute
  -- them accordingly
  mapStyles scs sss = hcat (concatMap (\(sctx, ss) ->
                       [ ppr (space, foldr1 f ss)
                       , pprParen (lCtxLabelCombine' f ss sctx, map, f) ])
                       $ zip scs (tails sss))

-- Outputting AppDs: ----------------------------------------------------------

pprAppD :: ( L_Ctx ShowSettings
           , KeywordMap
           , ShowSettings -> ShowSettings -> ShowSettings )
           -> Doc
pprAppD (L_AppD (NIL, [], ss), map, _) = pprKeyword' "nil" map ss
pprAppD (L_AppD (CONS, [var@L_Var{}, L_AppD (NIL, [], _)], ss), map, f)
 = pprKeyword' "lbrack" map ss
    <> ppr (lCtxLabelCombine f ss var, map, f)
    <> pprKeyword' "rbrack" map ss
pprAppD (L_AppD (CONS, [var1@L_Var{}, var2@L_Var{}], ss), map, f)
 = pprKeyword' "lparen" map ss
    <> ppr (lCtxLabelCombine f ss var1, map, f)
    <> pprKeyword' "colon" map ss
    <> ppr (lCtxLabelCombine f ss var2, map, f)
    <> pprKeyword' "rparen" map ss

-- Use [,] notation if the list is of uniform atoms
-- Use (::) notation if the list is non-uniform or does
pprAppD x@(l@(L_AppD (CONS, _, ss)), map, f)
 | isUniformAtomicList l = pprKeyword' "lbrack" map ss
                            <> (hcat
                                 . intersperse (pprKeyword' "comma" map ss)
                                 . pprList') x
                            <> pprKeyword' "rbrack" map ss
 | isList l = (hcat
                . intersperse (ppr (space, ss)
                   <> pprKeyword' "colon" map ss
                   <> ppr (space, ss))
                . pprList) x

 | isMixedList l = pprParen (lCtxLabelCombine f ss h, map, f)
                    <> ppr (space, ss)
                    <> pprKeyword' "colon" map ss
                    <> ppr (space, ss)
                    <> pprParen (lCtxLabelCombine f ss t, map, f)
   where L_AppD (CONS, [h, t], _) = l
-- Anything else is an invalid datatype (currently)
pprAppD _ = invalidDT

-- Outputting Binds: ----------------------------------------------------------

instance Outputable (L_Bind ShowSettings) where
  ppr x = ppr (x, plainKeywords, inherit)

instance Outputable (L_Bind ShowSettings, KeywordMap) where
  ppr (x, map) = ppr (x, map, inherit)

instance Outputable ( L_Bind ShowSettings
                    , KeywordMap
                    , ShowSettings -> ShowSettings -> ShowSettings ) where
  ppr (L_Bind (ns, sctx, _, ss), map, f)
   = ppr (text ns, ss)
      <> ppr (space, ss)
      <> pprKeyword' "eq" map ss
      <> ppr (space, ss)
      <> ppr (lCtxLabelCombine f ss sctx, map, f)

-- Outputting Alts: -----------------------------------------------------------

instance Outputable (L_Alt ShowSettings) where
  ppr x = ppr (x, plainKeywords, inherit)

instance Outputable (L_Alt ShowSettings, KeywordMap) where
  ppr (x, map) = ppr (x, map, inherit)

instance Outputable ( L_Alt ShowSettings
                    , KeywordMap
                    , ShowSettings -> ShowSettings -> ShowSettings ) where
  ppr (L_Alt (con, nss, sctx, _, ss), map, f)
   = pprCon con nss map ss 0
      <> ppr (space, ss)
      <> pprKeyword' "arrow" map ss
      <> ppr (space, ss)
      <> ppr (lCtxLabelCombine f ss sctx, map, f)

-- Outputting Cons: -----------------------------------------------------------

-- This needs improving

pprCon :: Con -> [Name] -> KeywordMap -> ShowSettings -> Int -> Doc
pprCon VARIABLE   [ns] _   ss i = ppr (text ns, ss) <> spaces ss i
pprCon CONS       [ns] map ss i = pprKeyword' "lbrack" map ss
                                   <> text ns
                                   <> pprKeyword' "rbrack" map ss
                                   <> spaces ss i
pprCon CONS       nss  map ss i = pprKeyword' "lparen" map ss
                                   <> (hcat
                                       . intersperse (ppr (space, ss)
                                          <> pprKeyword' "colon" map ss
                                          <> ppr (space, ss))
                                       . fmap (ppr . (, ss) . text)) nss
                                   <> pprKeyword' "rparen" map ss
                                   <> spaces ss i
pprCon NIL        []   map ss i = pprKeyword' "nil" map ss <> spaces ss i
pprCon DEFAULT    []   map ss i = pprKeyword' "wildcard" map ss <> spaces ss i
pprCon (LITINT j) []   _   ss i = ppr (int j, ss) <> spaces ss i
pprCon (LITSTR s) []   _   ss i = ppr (text ('\"' : s ++ "\""), ss)
                                   <> spaces ss i
pprCon _          _    _   _  _ = invalidC

conWidth :: Con  -> [Name] -> Int
conWidth VARIABLE   [ns] = length ns
conWidth CONS       [ns] = length ns + 1
conWidth CONS       nss  = (sum $ fmap length nss) + 5
conWidth NIL        []   = 2
conWidth DEFAULT    []   = 1
conWidth (LITINT i) []   = length (show i)
conWidth (LITSTR s) []   = length s + 2
conWidth _          _    = 0

spaces :: ShowSettings -> Int -> Doc
spaces ss i = ppr (hcat $ replicate i space, ss)

-- Outputting Lists (sugar syntax): -------------------------------------------

pprList :: ( L_Ctx ShowSettings
           , KeywordMap
           , ShowSettings -> ShowSettings -> ShowSettings )
           -> [Doc]
pprList (L_AppD (NIL, [], ss), map, _) = [pprKeyword' "nil" map ss]
pprList (L_AppD (CONS, [sc1, sc2], ss), map, f) =
    ppr (lCtxLabelCombine f ss sc1, map, f)
  : pprList (lCtxLabelCombine f ss sc2, map, f)
pprList x = [ppr x]

-- As above but doesn't print nil
pprList' :: ( L_Ctx ShowSettings
            , KeywordMap
            , ShowSettings -> ShowSettings -> ShowSettings )
            -> [Doc]
pprList' (L_AppD (NIL, [], _), _, _) = []
pprList' (L_AppD (CONS, [sc1, sc2], ss), map, f)
 = ppr (lCtxLabelCombine f ss sc1, map, f)
    : pprList' (lCtxLabelCombine f ss sc2, map, f)
pprList' x = [ppr x]

-------------------------------------------------------------------------------
-- Helpers: --
-------------------------------------------------------------------------------

-- Convert operator name to infix by removing outer parentheses
toInfix :: [Char] -> [Char]
toInfix  = takeWhile (/= ')') . drop 1

-- Parenthesise if /not/ printably atomic
pprParen :: ( L_Ctx ShowSettings
            , KeywordMap
            , ShowSettings -> ShowSettings -> ShowSettings )
            -> Doc
pprParen x@(sctx, map, _) | atomic sctx = ppr x
                          | otherwise   = pprKeyword' "lparen" map ss
                                           <> ppr x
                                           <> pprKeyword' "rparen" map ss
                                          where ss = lCtxLabel sctx

-- Printably atomic
atomic           :: L_Ctx a -> Bool
atomic L_Var{}    = True
atomic L_LitInt{} = True
atomic L_LitStr{} = True
atomic L_Tick{}   = True
atomic L_AppD{}   = True -- This is okay for now because we only have lists
atomic L_Hole{}   = True
atomic L_CVar{}   = True
atomic  _         = False

-- Checking the form of list elements: --

isUniformAtomicList :: L_Ctx ShowSettings -> Bool
isUniformAtomicList (L_AppD (NIL, [], _))                  = True
isUniformAtomicList (L_AppD (CONS, [L_Var{}, sctx], _))    = isVarList sctx
isUniformAtomicList (L_AppD (CONS, [L_LitInt{}, sctx], _)) = isLitIntList sctx
isUniformAtomicList (L_AppD (CONS, [L_LitStr{}, sctx], _)) = isLitStrList sctx
isUniformAtomicList _                                      = False

{-
isAtomicList :: L_Ctx ShowSettings -> Bool
isAtomicList (L_AppD (NIL, [], _))                  = True
isAtomicList (L_AppD (CONS, [L_Var{}, sctx], _))    = isAtomicList sctx
isAtomicList (L_AppD (CONS, [L_LitInt{}, sctx], _)) = isAtomicList sctx
isAtomicList (L_AppD (CONS, [L_LitStr{}, sctx], _)) = isAtomicList sctx
isAtomicList _                                      = False
-}

isVarList :: L_Ctx ShowSettings -> Bool
isVarList (L_AppD (NIL, [], _))                  = True
isVarList (L_AppD (CONS, [L_Var{}, sctx], _))    = isVarList sctx
isVarList _                                      = False

isLitIntList :: L_Ctx ShowSettings -> Bool
isLitIntList (L_AppD (NIL, [], _))                  = True
isLitIntList (L_AppD (CONS, [L_LitInt{}, sctx], _)) = isLitIntList sctx
isLitIntList _                                      = False

isLitStrList :: L_Ctx ShowSettings -> Bool
isLitStrList (L_AppD (NIL, [], _))                  = True
isLitStrList (L_AppD (CONS, [L_LitStr{}, sctx], _)) = isLitStrList sctx
isLitStrList _                                      = False

isMixedList :: L_Ctx ShowSettings -> Bool
isMixedList (L_AppD (NIL, [], _))                = True
isMixedList (L_AppD (CONS, [_, s@L_AppD{}], _))  = isMixedList s
isMixedList (L_AppD (CONS, [_, _], _))           = True
isMixedList _                                    = False

isList :: L_Ctx ShowSettings -> Bool
isList (L_AppD (NIL, [], _))         = True
isList (L_AppD (CONS, [_, sctx], _)) = isList sctx
isList _                             = False
