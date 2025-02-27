module Heck.Compiler.Types where

import Control.Monad.State (State)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type JsName = String

type JsArg = Int

data JsStmt
  = JsLet JsName JsExpr
  | JsIf JsExpr JsStmt
  | JsReturn JsExpr
  | JsBlock [JsStmt]
  | JsExpr JsExpr
  | JsContinue
  | JsAssignment JsName JsExpr
  | JsWhileTrue [JsStmt]
  deriving (Eq, Show)

data JsExpr
  = JsLit JsLit
  | JsVar JsName
  | JsInline String
  | JsFunc JsArg JsExpr
  | JsBin JsName JsExpr JsExpr
  | JsNeg JsExpr
  | JsImmediatelyEvaluatedLambda [JsStmt]
  | JsCall JsExpr JsExpr
  deriving (Eq, Show)

data JsLit
  = JsUndefined
  | JsNull
  | JsBool Bool
  | JsInt Int
  | JsDouble Double
  | JsString String
  deriving (Eq, Show)

type FuncPart = (Maybe JsExpr, JsExpr)

data CompilerState = CompilerState
  { csArgCounter :: JsArg,
    csArities :: Map JsName Int,
    csFuncParts :: Map JsName [JsStmt],
    csVarBindings :: Map JsName JsExpr,
    cpFuncStmts :: Map JsName [FuncPart],
    cpIsTailRec :: Map JsName Bool
  }
  deriving (Eq, Show)

defaultState :: CompilerState
defaultState = CompilerState 0 M.empty M.empty M.empty M.empty M.empty

pushArg :: CompilerState -> (JsArg, CompilerState)
pushArg (CompilerState counter arities funcParts binds fs tr) =
  (counter, CompilerState (counter + 1) arities funcParts binds fs tr)

popArg :: CompilerState -> CompilerState
popArg (CompilerState 0 _ _ _ _ _) = error "Can't pop argument from empty list!"
popArg (CompilerState cnt arities funcParts varBindings fs tr) =
  CompilerState (cnt - 1) arities funcParts varBindings fs tr

addArity :: JsName -> Int -> CompilerState -> CompilerState
addArity name arity (CompilerState cnt arities funcParts varBindings fs tr) =
  CompilerState cnt (M.insert name arity arities) funcParts varBindings fs tr

addFunctionStatement :: JsName -> JsStmt -> CompilerState -> CompilerState
addFunctionStatement name stmt (CompilerState cnt arities funcParts varBindings fs tr) =
  CompilerState cnt arities (M.alter (Just . (stmt :) . fromMaybe []) name funcParts) varBindings fs tr

addVarBinding :: JsName -> JsExpr -> CompilerState -> CompilerState
addVarBinding name argCnt (CompilerState cnt arities funcParts varBindings fs tr) =
  CompilerState cnt arities funcParts (M.insert name argCnt varBindings) fs tr

addFuncStmts :: JsName -> Maybe JsExpr -> JsExpr -> CompilerState -> CompilerState
addFuncStmts name cond expr (CompilerState cnt arities funcParts varBindings funcStmts tr) =
  CompilerState cnt arities funcParts varBindings (M.alter adjuster name funcStmts) tr
  where
    adjuster :: Maybe [FuncPart] -> Maybe [FuncPart]
    adjuster Nothing = Just [(cond, expr)]
    adjuster (Just xs) = Just (xs <> [(cond, expr)])

setTailRec :: JsName -> CompilerState -> CompilerState
setTailRec name (CompilerState c a fp v fs tailRecs) =
  CompilerState c a fp v fs (M.insert name True tailRecs)

type Heck = State CompilerState
