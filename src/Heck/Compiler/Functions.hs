module Heck.Compiler.Functions
  ( getFunctionDefs,
    compileFunBinds,
    compilePatBind,
    processTypeSignatures,
  )
where

import Control.Monad.State (MonadState (get, put), gets, modify)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio (denominator, numerator)
import Heck.Compiler.Types
import Heck.Utils (concatMapM, mapIndexed, mapIndexedM_, replace)
import Language.Haskell.Exts
  ( Binds,
    Literal,
    Match (InfixMatch, Match),
    Name (Ident, Symbol),
    Pat (PLit, PVar),
    Rhs (..),
    Sign (Negative),
    Type (..),
  )
import qualified Language.Haskell.Exts as Hs

asArg :: Int -> JsExpr
asArg i = JsVar ('$' : show i)

toJsInt :: Integer -> JsExpr
toJsInt = JsLit . JsInt . fromInteger

toJsFrac :: Rational -> JsExpr
toJsFrac frac = JsBin "/" num denom
  where
    num = toJsInt (numerator frac)
    denom = toJsInt (denominator frac)

jsifyLiteral :: Literal l -> JsExpr
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

getConditionForPattern :: Int -> Pat l -> Maybe JsExpr
getConditionForPattern _ PVar {} = Nothing
getConditionForPattern argI (PLit _ (Negative _) lit) =
  Just $ JsBin "==" (asArg argI) (JsNeg (jsifyLiteral lit))
getConditionForPattern argI (PLit _ _ lit) =
  Just $ JsBin "==" (asArg argI) (jsifyLiteral lit)
getConditionForPattern _ _ = Nothing

getConditionsForPatterns :: (Show l) => [Pat l] -> Maybe JsExpr
getConditionsForPatterns [] = Nothing
getConditionsForPatterns ls =
  let conds = catMaybes (mapIndexed getConditionForPattern ls)
   in case conds of
        [] -> Nothing
        [x] -> Just x
        _ -> Just $ foldr1 (JsBin "&&") conds

makeJsName :: Name l -> JsName
makeJsName (Ident _ name) = replace '\'' '$' name
makeJsName (Symbol _ name) = name -- FIXME: this doesn't actually work :3

getNameFromQName :: Hs.QName l -> JsName
getNameFromQName (Hs.Qual _ _ name) = makeJsName name
getNameFromQName (Hs.UnQual _ name) = makeJsName name
getNameFromQName (Hs.Special _ _) = "unknown"

getNameFromQOp :: Hs.QOp l -> JsName
getNameFromQOp (Hs.QVarOp _ name) = getNameFromQName name
getNameFromQOp (Hs.QConOp _ name) = getNameFromQName name

compileExpr :: Hs.Exp l -> Heck JsExpr
compileExpr (Hs.Lit _ lit) = return $ jsifyLiteral lit
compileExpr (Hs.Var _ qname) = do
  let jsName = getNameFromQName qname
  bindings <- gets csVarBindings
  return $ M.findWithDefault (JsVar jsName) jsName bindings
compileExpr (Hs.InfixApp _ l op r) =
  JsBin (getNameFromQOp op) <$> compileExpr l <*> compileExpr r
compileExpr (Hs.App _ f v) = JsCall <$> compileExpr f <*> compileExpr v
compileExpr (Hs.Paren _ inner) = compileExpr inner
compileExpr (Hs.NegApp _ expr) = JsNeg <$> compileExpr expr
compileExpr _ = return $ JsLit JsNull

compileRhs :: Rhs l -> Heck JsExpr
compileRhs (Hs.GuardedRhss _ _) = return $ JsLit (JsString "NO")
compileRhs (Hs.UnGuardedRhs _ expr) = compileExpr expr

registerPattern :: Int -> Pat l -> Heck ()
registerPattern i (PVar _ name) = modify $ addVarBinding (makeJsName name) (asArg i)
registerPattern _ _ = return ()

isTailRec :: JsName -> JsExpr -> Bool
isTailRec name callee =
  case callee of
    JsVar calleeName -> calleeName == name
    JsCall subCallee _ -> isTailRec name subCallee
    _ -> False

hasTailRec :: JsName -> [FuncPart] -> Bool
hasTailRec _ [] = False
hasTailRec name ((_, callee) : xs) = isTailRec name callee || hasTailRec name xs

tailRecPart :: Int -> JsName -> FuncPart -> [JsStmt]
tailRecPart arity name part@(_, callee) =
  if not (isTailRec name callee)
    then [makeStmt part]
    else makeTailRecStmt arity name part

calleeIsTheFunc :: JsName -> JsExpr -> Bool
calleeIsTheFunc name (JsVar callee) = name == callee
calleeIsTheFunc _ _ = False

makeTailRecStmt :: Int -> JsName -> FuncPart -> [JsStmt]
makeTailRecStmt arity name (Just cond, expr) =
  [JsIf cond (JsBlock (makeTailRecStmts arity name expr))]
makeTailRecStmt arity name (Nothing, expr) =
  makeTailRecStmts arity name expr

makeTailRecStmts :: Int -> JsName -> JsExpr -> [JsStmt]
makeTailRecStmts arity name expr = makeTailRecStmts' arity name expr 0 []

makeFinalArgAssignments :: Int -> [JsStmt]
makeFinalArgAssignments i =
  map (\n -> JsAssignment ('$' : show n) (JsVar ("$$" <> show n))) [0 .. i]

makeTailRecStmts' :: Int -> JsName -> JsExpr -> Int -> [JsStmt] -> [JsStmt]
makeTailRecStmts' _ _ (JsVar _) _ stmts = stmts <> [JsContinue]
makeTailRecStmts' arity name (JsCall callee arg) i stmts =
  if calleeIsTheFunc name callee
    then stmts <> (JsLet ("$$" <> show (arity - i - 1)) arg : makeFinalArgAssignments i) <> [JsContinue]
    else JsLet ("$$" <> show (arity - i - 1)) arg : makeTailRecStmts' arity name callee (i + 1) stmts
makeTailRecStmts' _ _ _ _ _ = error "makeTailRecStmt' was called with something weird. This shouldn't happen :("

makeStmt :: FuncPart -> JsStmt
makeStmt (Just cond, expr) = JsIf cond (JsReturn expr)
makeStmt (Nothing, expr) = JsReturn expr

optimizeTailRec :: Int -> JsName -> [FuncPart] -> [JsStmt]
optimizeTailRec arity name parts =
  if hasTailRec name parts
    then [JsWhileTrue $ concatMap (tailRecPart arity name) parts]
    else map makeStmt parts

getFunctionDef :: JsName -> [FuncPart] -> Heck [JsStmt]
getFunctionDef name parts = do
  maybeI <- gets (M.lookup name . csArities)
  let i = fromJust maybeI
      stmts = optimizeTailRec i name parts
  return [JsLet name $ getFunctionDef' i 0 (JsImmediatelyEvaluatedLambda stmts)]
  where
    getFunctionDef' :: Int -> Int -> JsExpr -> JsExpr
    getFunctionDef' i curr expr
      | curr == i = expr
      | otherwise = JsFunc curr (getFunctionDef' i (curr + 1) expr)

getFunctionDefs :: Heck [JsStmt]
getFunctionDefs = do
  defs <- gets cpFuncStmts
  concatMapM (uncurry getFunctionDef) (M.assocs defs)

-- JsFunc (makeJsName name) (JsLit JsNull)
compileMatch :: (Show l) => Name l -> [Pat l] -> Rhs l -> Maybe (Binds l) -> Heck [JsStmt]
compileMatch name pats rhs _ = do
  let cond = getConditionsForPatterns pats
  oldState <- get
  modify (addArity (makeJsName name) (length pats))
  mapIndexedM_ registerPattern pats
  jsRhs <- compileRhs rhs
  put oldState
  modify $ addFuncStmts (makeJsName name) cond jsRhs
  return []

compileFunBind :: (Show l) => Match l -> Heck [JsStmt]
compileFunBind InfixMatch {} = return []
compileFunBind (Match _ name pats rhs binds) =
  compileMatch name pats rhs binds

compileFunBinds :: (Show l) => [Match l] -> Heck [JsStmt]
compileFunBinds = concatMapM compileFunBind

compilePatBind :: Hs.Pat l -> Hs.Rhs l -> Maybe (Hs.Binds l) -> Heck [JsStmt]
compilePatBind (Hs.PVar _ name) rhs _ =
  (\x -> [JsLet (makeJsName name) x]) <$> compileRhs rhs
compilePatBind _ _ _ = return []

getFunctionArity :: Type l -> Int
getFunctionArity ty = getFunctionArity' ty 0

getFunctionArity' :: Type l -> Int -> Int
getFunctionArity' (TyFun _ _ funRet) a = getFunctionArity' funRet (a + 1)
getFunctionArity' (TyForall _ _ _ t) a = getFunctionArity' t a
getFunctionArity' _ a = a

processTypeSignatures :: [Name l] -> Type l -> Heck [JsStmt]
processTypeSignatures names typeSig =
  processTypeSignatures' names (getFunctionArity typeSig)
  where
    processTypeSignatures' :: [Name l] -> Int -> Heck [JsStmt]
    processTypeSignatures' [] _ = return []
    processTypeSignatures' (n : ns) arity =
      modify (addArity (makeJsName n) arity) >> processTypeSignatures' ns arity