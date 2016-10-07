module Lib where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.RefactorBase

import SrcLoc

import Control.Reference hiding (element)
import Data.Generics.Uniplate.Data
import Debug.Trace

someFunc = tryOutHello "Test" "6:5-6:16"

tryOutHello moduleName sp = tryRefactor (localRefactoring $ helloRefactor (readSrcSpan (toFileName "." moduleName) sp)) moduleName

helloRefactor :: Domain dom => RealSrcSpan -> LocalRefactoring dom
helloRefactor sp = return . (nodesContained sp .- helloExpr)

helloExpr :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e
