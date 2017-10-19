
module ParamRefine
  (paramsRefine  -- Top-level function for refining parameters.

    -- Parameter refiners: --

  , anyCmdNameRefine      -- Accepts any command name.
  , bindSrcCodeRefine     -- Accepts a single binding.
  , cBindSrcCodeRefine    -- Accepts a CBind defined in source code.
  , cVarSrcCodeRefine     -- Accepts a CVar defined in source code.
  , caseSrcCodeRefine     -- Accepts a Case defined in source code.
  , cmdNameRefine         -- Accepts a specific command name.
  , ctxKindRefine         -- Accepts any context kind.
  , ctxPatSrcCodeRefine   -- Accepts any context pattern source code.
  , ctxSrcCodeRefine      -- Accepts a Ctx defined in source code.
  , ctxSrcNameRefine      -- Accepts a context source name (uppercase)
  , fileRefine            -- Accepts a filepath.
  , numberRefine          -- Accepts a positive integer.
  , propRefine            -- Accepts a proof proposition.
  , redexSrcCodeRefine    -- Accepts a reducible expression.
  , relRefine             -- Accepts a relation.
  , relRefine'            -- As above, but returns a Param.
  , tBindSrcCodeRefine    -- Accepts a CBind defined in source code.
  , termPatSrcCodeRefine  -- Accepts a context pattern without holes.
  , termSrcCodeRefine     -- Accepts a context defined in source code without holes.
  , termSrcNameRefine     -- Accepts a term source name (lowercase)

  ) where 

import CmdAST               (LocatedRawParam(..), RawParam(..), Param(..))
import CmdError             (ParamError(..), sortParamErrors)
import CtxKind              (strToCtxKind)
import CtxPatAST            (UPat(..))
import CtxUtils             (isRedex)
import IndentationParserLib (runParser)
import ParsingUtils         (expectedError)
import Relations            (Relation, strToRel)
import Universes            (U(..))

import qualified CtxParser       as P
import qualified CtxPatParser    as PP
import qualified SourceParserLib as SPL

import Data.Char   (isUpper)
import Data.Either (isRight, lefts)
import Data.List   (partition, permutations)

{-
  <TO-DO>: - Fix bindsSrcCodeRefine, as only works for one bind ATM.

  Information:
  -----------------------------------------------------------------------------
  - A module for refining command parameters;
  - See CmdRefine for a detailed discussion on the refinement process.
-}

-- Top-level function: --------------------------------------------------------

{-
  Refine a list of raw command parameters using a list of refiners, which is 
  zipped with it. Each refiner is responsible for precisely one param. 
  and either parses the param. successfully, converting it from a 
  LocatedRawParam to a Param, or returns a parameter error typically 
  specifying information regarding its /expected/ parameter; 

  We attempt to return the most appropriate error message if the refiners 
  fail, see CmdError: sortParamErrors.
-}
paramsRefine :: [LocatedRawParam] 
                -> [[LocatedRawParam -> Either [ParamError] Param]] 
                -> Either [ParamError] [Param]
paramsRefine lrps funs =  
  if null funs'
     then Left $ fmap (\lrp -> InvalidParam (show . par $ lrp) (pos lrp)) lrps
     else case fmap sequence succ of
           (Right ps : _) -> Right ps  -- Take the first successful parse if 
                                       -- multiple occur.
           _              -> Left . sortParamErrors 
                                  . fmap (concat . lefts) 
                                  $ fail
    where 
      (succ, fail) = partition (all isRight) 
                      [ zipWith ($) fs perm  
                      | perm <- permutations lrps, fs <- funs' ]
      funs' = filter (\fs -> length fs == l) funs
      l     = length lrps 
                        
-- Individual parameter refiners: ---------------------------------------------

-- Prop.
propRefine :: LocatedRawParam -> Either [ParamError] Param 
propRefine (LocatedRawParam rp@(RawProp p1 pr p2) pos) = combine psRef prRef  
  where 
    psRef = paramsRefine [ (LocatedRawParam p1 pos) 
                         , (LocatedRawParam p2 pos) ] opts
    prRef = relRefine (LocatedRawParam pr pos)
    opts  = [ [termSrcNameRefine, termSrcNameRefine] 
            , [termSrcCodeRefine, termSrcCodeRefine]
            , [termSrcCodeRefine, termSrcNameRefine]
            , [termSrcNameRefine, termSrcCodeRefine] ]

    -- Combine individual raw params. to build a prop,
    -- or combine error messages.
    combine (Left es1)       (Left es2) = Left (es1 ++ es2)
    combine (Left es)        Right{}    = Left es
    combine Right{}          (Left es)  = Left es
    combine (Right [p1, p2]) (Right r)  = Right (Prop p1 r p2)
    combine _                _          = Left [InvalidParam (expectedError 
                                          (show rp) "Prop") pos]
propRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "Prop") pos]

-- Term source names.
termSrcNameRefine :: LocatedRawParam -> Either [ParamError] Param
termSrcNameRefine (LocatedRawParam rp@(RawSrcName s) pos) = 
  case runParser SPL.tBindName s of 
    Just ns -> Right (TermSrcName ns)
    Nothing -> Left [InvalidParam (expectedError (show rp) "SrcName :: Term") pos]
termSrcNameRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError (show rp) "SrcName :: Term") pos]

-- Context source names.
ctxSrcNameRefine :: LocatedRawParam -> Either [ParamError] Param
ctxSrcNameRefine (LocatedRawParam (RawSrcName (c : cs)) _)    
  | isUpper c = Right (CtxSrcName (c : cs))
ctxSrcNameRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "SrcName :: Ctx") pos]

-- TBind source code.
tBindSrcCodeRefine :: LocatedRawParam -> Either [ParamError] Param
tBindSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) =
  case runParser P.tBind s of
    Just tBind -> Right (TermSrcCode $ UGBind tBind)
    Nothing    -> Left [InvalidParam (expectedError (show rp) "SrcCode :: TBind") pos]
tBindSrcCodeRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "SrcCode :: CBind") pos]

-- CBind source code.
cBindSrcCodeRefine :: LocatedRawParam -> Either [ParamError] Param
cBindSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) =  
  case runParser P.cBind s of
    Just cBind -> Right (CtxSrcCode $ UGBind cBind)
    Nothing    -> Left [InvalidParam (expectedError (show rp) "SrcCode :: CBind") pos]
cBindSrcCodeRefine (LocatedRawParam rp pos)    =  Left [InvalidParam (expectedError 
                                                   (show rp) "SrcCode :: CBind") pos]
-- CVar source code.
cVarSrcCodeRefine :: LocatedRawParam -> Either [ParamError] Param
cVarSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) = 
  case runParser P.cVar s of
    Just cVar -> Right (CtxSrcCode $ UCtx cVar)
    Nothing   -> Left [InvalidParam (expectedError (show rp) "SrcCode :: CVar") pos]
cVarSrcCodeRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "SrcCode :: CVar") pos]

-- Term source code.
termSrcCodeRefine :: LocatedRawParam -> Either [ParamError] Param
termSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) =  
  case runParser P.term s of
    Just t  -> Right (TermSrcCode $ UCtx t)
    Nothing -> Left [InvalidParam (expectedError (show rp) "SrcCode :: Term") pos]
termSrcCodeRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError (show rp) "SrcCode :: Term") pos]

-- Case source code.
caseSrcCodeRefine :: LocatedRawParam -> Either [ParamError] Param     
caseSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) = 
  case runParser P.esac s of
    Just t  -> Right (TermSrcCode $ UCtx t)
    Nothing -> Left [InvalidParam (expectedError (show rp) "SrcCode :: Case") pos]
caseSrcCodeRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "SrcCode :: Case") pos]         

-- Redex source code.
redexSrcCodeRefine :: LocatedRawParam -> Either [ParamError] Param 
redexSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) = 
  case runParser P.term s of
    Just t  | isRedex t -> Right (TermSrcCode $ UCtx t)
    _ -> Left [InvalidParam (expectedError (show rp) "SrcCode :: Redex") pos]
redexSrcCodeRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "SrcCode :: Redex") pos]  

-- List of bindings source code.
-- We can only do one at a time for now.
bindSrcCodeRefine  :: LocatedRawParam -> Either [ParamError] Param
bindSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) = 
  case runParser P.bind' s of
    Just b -> Right (TermSrcCode $ UBind b)
    Nothing -> Left [InvalidParam (expectedError (show rp) "SrcCode :: Bind") pos]
bindSrcCodeRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError (show rp) "SrcCode :: Bind") pos]

{-
bindsSrcCodeRefine  :: LocatedRawParam -> Either [ParamError] Param
bindsSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) = 
  case runParser P.bind' s of
    Just b -> Right (TermSrcCode $ UBind b)
    Nothing -> Left [InvalidParam (expectedError (show rp) "SrcCode :: Bind") pos]
bindsSrcCodeRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError (show rp) "SrcCode :: Bind") pos]
-}

-- Context source code.
ctxSrcCodeRefine  :: LocatedRawParam -> Either [ParamError] Param
ctxSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) = 
  case runParser P.ctx s of
    Just ctx -> Right (CtxSrcCode $ UCtx ctx)
    Nothing  -> Left [InvalidParam (expectedError (show rp) "SrcCode :: Ctx") pos]
ctxSrcCodeRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError (show rp) "SrcCode :: Ctx") pos]

-- Term pattern source code.
termPatSrcCodeRefine  :: LocatedRawParam -> Either [ParamError] Param
termPatSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) =
  case runParser PP.term s of 
    Just t  -> Right (TermPatSrcCode $ UCtxPat t)
    Nothing -> Left [InvalidParam (expectedError (show rp) "PatSrcCode :: Term") pos]
termPatSrcCodeRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError (show rp) "PatSrcCode :: Term") pos]

-- Context pattern source code.
ctxPatSrcCodeRefine :: LocatedRawParam -> Either [ParamError] Param
ctxPatSrcCodeRefine (LocatedRawParam rp@(RawSrcCode s) pos) = 
  case runParser PP.ctx s of 
    Just pctx -> Right (CtxPatSrcCode $ UCtxPat pctx)
    Nothing   -> Left [InvalidParam (expectedError (show rp) "PatSrcCode :: Ctx") pos]
ctxPatSrcCodeRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError (show rp) "PatSrcCode :: Ctx") pos]

-- Specific command names.
cmdNameRefine :: [String] -> LocatedRawParam -> Either [ParamError] Param
cmdNameRefine ss (LocatedRawParam (RawCmdName s) _) 
  | s `elem` ss = Right (CmdName s)
cmdNameRefine _ (LocatedRawParam rp pos) = Left [InvalidParam (show rp) pos]

-- Any command name.
anyCmdNameRefine :: LocatedRawParam -> Either [ParamError] Param
anyCmdNameRefine (LocatedRawParam (RawCmdName s) _) = Right (CmdName s)
anyCmdNameRefine (LocatedRawParam rp pos) =  
  Left [InvalidParam (expectedError  (show rp) "CmdName") pos]

-- Relations.
relRefine :: LocatedRawParam -> Either [ParamError] Relation 
relRefine (LocatedRawParam rp@(RawCmdName s) pos) =
  case strToRel s of 
    Just rel -> Right rel
    Nothing  -> Left [InvalidParam (expectedError (show rp) "Rel") pos]
relRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "Rel") pos]

-- Relations: returns a Param.
relRefine' :: LocatedRawParam -> Either [ParamError] Param 
relRefine' (LocatedRawParam rp@(RawCmdName s) pos) =
  case strToRel s of 
    Just rel -> Right $ Rel rel
    Nothing  -> Left [InvalidParam (expectedError (show rp) "Rel") pos]
relRefine' (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "Rel") pos]

-- Numbers.
numberRefine :: LocatedRawParam -> Either [ParamError] Param 
numberRefine (LocatedRawParam (RawNumber i) _)  = Right (Number i)
numberRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "Number") pos]

-- Files.
fileRefine :: LocatedRawParam -> Either [ParamError] Param 
fileRefine (LocatedRawParam (RawFile fp) _) = Right (File fp)
fileRefine (LocatedRawParam rp pos) = 
  Left [InvalidParam (expectedError (show rp) "File") pos]

-- Context kinds.
ctxKindRefine :: LocatedRawParam -> Either [ParamError] Param    
ctxKindRefine (LocatedRawParam  rp@(RawCmdName s) pos) =  
  case strToCtxKind s of 
    Just k  -> Right (CtxKind k)
    Nothing -> Left [InvalidParam (expectedError (show rp) "CtxKind") pos]
ctxKindRefine (LocatedRawParam rp pos) = Left [InvalidParam (show rp) pos]

