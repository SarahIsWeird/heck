module Heck.Compiler
  ( runCompiler,
    compileModule,
    JsStmt (..),
    JsExpr (..),
    JsLit (..),
  )
where

import Control.Monad.State (evalState)
import Heck.Compiler.Functions
import Heck.Compiler.Types
import Heck.Stuff (HsModule (..))
import Heck.Utils (concatMapM)
import Language.Haskell.Exts hiding (binds)

-- Not actually sure if we need this to be a list, but it prevents me
-- having to rewrite a lot of shit
compileDeclaration :: (Show l) => Decl l -> Heck [JsStmt]
compileDeclaration (FunBind _ matches) = compileFunBinds matches
compileDeclaration (PatBind _ pat rhs binds) = compilePatBind pat rhs binds
compileDeclaration (TypeSig _ names typeSig) = processTypeSignatures names typeSig
compileDeclaration _ = return []

compileModule :: HsModule -> Heck [JsStmt]
compileModule hsm = do
  decls <- concatMapM compileDeclaration (hsmDecls hsm)
  funcDefs <- getFunctionDefs
  return $ funcDefs <> decls

runCompiler :: Heck [JsStmt] -> [JsStmt]
runCompiler c = evalState c defaultState
