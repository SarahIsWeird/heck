module Heck.Compiler.Types where

import Heck.Compiler.Misc
import qualified Language.Haskell.Exts as Hs
import Heck.Utils (concatMapM)
import Data.List (singleton)

compileTypeDecl :: Hs.DeclHead l -> Hs.Type l -> Heck [JsStmt]
compileTypeDecl (Hs.DHead _ _) _ = return [] -- Ostensibly this is just an alias, so we don't care (yet).
compileTypeDecl (Hs.DHApp {}) _ = return [] -- Same same
compileTypeDecl (Hs.DHInfix {}) _ = error "Infix types aren't supported yet."
compileTypeDecl (Hs.DHParen {}) _ = error "Parenthesized types aren't supported yet."

compileDataDecl :: Hs.DataOrNew l -> Maybe (Hs.Context l) -> Hs.DeclHead l -> [Hs.QualConDecl l] -> [Hs.Deriving l] -> Heck [JsStmt]
compileDataDecl _ (Just _) _ _ _ = error "Context isn't allowed yet."
compileDataDecl _ _ _ _ (_ : _) = error "Derivation isn't allowed yet."
-- The issue with newtype is that it is erased at runtime.
compileDataDecl (Hs.NewType _) _ _ _ _ = error "newtype isn't allowed yet."
-- Write out the other args to make sure we didn't miss any, but weren't over-zealous above either
compileDataDecl (Hs.DataType _) Nothing declHead conDecls [] = makeConstructors declHead conDecls

makeConstructors :: Hs.DeclHead l -> [Hs.QualConDecl l] -> Heck [JsStmt]
makeConstructors (Hs.DHInfix {}) _ = error "Infix types aren't allowed yet."
makeConstructors (Hs.DHParen {}) _ = error "Parenthesized types aren't allowed yet."
makeConstructors _ decls = concatMapM makeConstructor decls

makeConstructor :: Hs.QualConDecl l -> Heck [JsStmt]
makeConstructor (Hs.QualConDecl _ (Just _) _ _) = error "Varbinds for constructors aren't allowed yet."
makeConstructor (Hs.QualConDecl _ _ (Just _) _) = error "Contexts for constructors aren't allowed yet."
makeConstructor (Hs.QualConDecl _ _ _ (Hs.InfixConDecl {})) = error "Infix data constructors aren't allowed yet."
makeConstructor (Hs.QualConDecl _ _ _ (Hs.ConDecl _ name types)) =
  singleton <$> makeSimpleConstructor name (length types) (map (\i -> '$' : show i) [0..(length types - 1)])
makeConstructor (Hs.QualConDecl _ _ _ (Hs.RecDecl _ name fieldDecls)) =
  makeRecordConstructor name fieldDecls

makeRecordConstructor :: Hs.Name l -> [Hs.FieldDecl l] -> Heck [JsStmt]
makeRecordConstructor name fieldDecls = do
  let fields = concatMap (\(Hs.FieldDecl _ names _) -> names) fieldDecls
  simpleCons <- makeSimpleConstructor name (length fields) (map makeJsName fields)
  getters <- mapM makeGetter fields
  return $ simpleCons : getters

makeGetter :: Hs.Name l -> Heck JsStmt
makeGetter name = return $ JsLet (makeJsName name) $ makeFunction 1 0 $ JsDot (JsVar "$0") (makeJsName name)

makeSimpleConstructor :: Hs.Name l -> Int -> [JsName] -> Heck JsStmt
makeSimpleConstructor name arity dataNames = 
  let funcBinding = JsLet (makeJsName name) func
      func = makeFunction arity 0 obj
      obj = JsObject $ map (\i -> (dataNames !! i, asArg i)) [0..(arity - 1)]
  in return funcBinding
