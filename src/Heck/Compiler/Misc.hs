module Heck.Compiler.Misc where

import Control.Monad.State (State)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.Exts as Hs
import Data.Ratio
import Heck.Utils

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
  | JsObject [(JsName, JsExpr)]
  | JsDot JsExpr JsName
  deriving (Eq, Show)

data JsLit
  = JsUndefined
  | JsNull
  | JsBool Bool
  | JsInt Int
  | JsDouble Double
  | JsString String
  deriving (Eq, Show)

asArg :: Int -> JsExpr
asArg i = JsVar ('$' : show i)

toJsInt :: Integer -> JsExpr
toJsInt = JsLit . JsInt . fromInteger

toJsFrac :: Rational -> JsExpr
toJsFrac frac = JsBin "/" num denom
  where
    num = toJsInt (numerator frac)
    denom = toJsInt (denominator frac)

jsifyLiteral :: Hs.Literal l -> JsExpr
jsifyLiteral (Hs.Char _ c _) = JsLit (JsString (c : ""))
jsifyLiteral (Hs.String _ s _) = JsLit (JsString s)
jsifyLiteral (Hs.Int _ i _) = toJsInt i
jsifyLiteral (Hs.Frac _ n _) = toJsFrac n
jsifyLiteral (Hs.PrimChar _ c _) = JsLit (JsString (c : ""))
jsifyLiteral (Hs.PrimString _ s _) = JsLit (JsString s)
jsifyLiteral (Hs.PrimInt _ i _) = toJsInt i
jsifyLiteral (Hs.PrimWord _ i _) = toJsInt i
jsifyLiteral (Hs.PrimFloat _ n _) = toJsFrac n
jsifyLiteral (Hs.PrimDouble _ n _) = toJsFrac n

makeJsName :: Hs.Name l -> JsName
makeJsName (Hs.Ident _ name) = replace '\'' '$' name
makeJsName (Hs.Symbol _ name) = name -- FIXME: this doesn't actually work :3

getNameFromQName :: Hs.QName l -> JsName
getNameFromQName (Hs.Qual _ _ name) = makeJsName name
getNameFromQName (Hs.UnQual _ name) = makeJsName name
getNameFromQName (Hs.Special _ _) = "unknown"

getNameFromQOp :: Hs.QOp l -> JsName
getNameFromQOp (Hs.QVarOp _ name) = getNameFromQName name
getNameFromQOp (Hs.QConOp _ name) = getNameFromQName name

makeFunction :: Int -> Int -> JsExpr -> JsExpr
makeFunction to from expr
  | from == to = expr
  | otherwise = JsFunc from (makeFunction to (from + 1) expr)

type FuncPart = (Maybe JsExpr, JsExpr)

data CompilerState = CompilerState
  { csArgCounter :: JsArg,
    csArities :: Map JsName Int,
    csVarBindings :: Map JsName JsExpr,
    cpFuncStmts :: Map JsName [FuncPart]
  }
  deriving (Eq, Show)

defaultState :: CompilerState
defaultState = CompilerState 0 M.empty M.empty M.empty

addArity :: JsName -> Int -> CompilerState -> CompilerState
addArity name arity state@CompilerState { csArities = arities } =
  state { csArities = M.insert name arity arities }

addVarBinding :: JsName -> JsExpr -> CompilerState -> CompilerState
addVarBinding name argCnt state@CompilerState { csVarBindings = varBindings } =
  state { csVarBindings = M.insert name argCnt varBindings }

addFuncStmts :: JsName -> Maybe JsExpr -> JsExpr -> CompilerState -> CompilerState
addFuncStmts name cond expr state@CompilerState { cpFuncStmts = funcStmts } =
  state { cpFuncStmts = M.alter adjuster name funcStmts }
  where
    adjuster :: Maybe [FuncPart] -> Maybe [FuncPart]
    adjuster Nothing = Just [(cond, expr)]
    adjuster (Just xs) = Just (xs <> [(cond, expr)])

type Heck = State CompilerState
