
module Test where 

import qualified CtxParser as CP 
import qualified CtxPatParser as CPP
import CtxPatMatch
import CtxPP 
import CtxPatPP
import CtxAST
import CtxPatAST
import IndentationParserLib 
import SourceParserLib
import CtxGen
import CtxEqLib
import Control.Monad.Reader
import Data.List 
import Utils
import Subst
import Runners
import Normalisation
import Universes
import Navigation
import Crumb
import TransUtils
-- import CtxASTEq
import KureMonad
import CtxShowAST
import AbstractMachine

import Language.KURE
import Control.Arrow

main = do 
         --x <- readFile "src/Testing/inp2"
         y <- readFile "src/Testing/fastrev"
         --z <- readFile "src/Testing/inp3"
         --let Just c1 = runParser CP.ctx x 
         let Just c2 = runParser CP.ctx y
         -- let Just sub = runParser CP.ctx y
         --let Just pctx2 = runParser CPP.ctx z
         --let lib = CtxEqLib [] [] [] []


         --Prelude.print $ runReader (stdCtxNest (autoStdCtxs f) ctx) lib
         --Prelude.print $ ctxPatGen [pctx] ctx
         
         
         let Right res = applyTCtx (tryR normaliseR) c2
         Prelude.print $ runEvalSugar res
         --Prelude.print $ isNormalisedCtx ctx

f = const True