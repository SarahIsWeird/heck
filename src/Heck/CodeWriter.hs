module Heck.CodeWriter where

import Heck.Compiler

writeStmt :: JsStmt -> String
writeStmt (JsLet name expr) = "let " <> name <> " = " <> writeExpr expr <> "\n"
writeStmt (JsIf cond stmt) = "if (" <> writeExpr cond <> ") " <> writeStmt stmt
writeStmt (JsReturn expr) = "return " <> writeExpr expr <> ";\n"
writeStmt (JsBlock stmts) = "{\n" <> concatMap writeStmt stmts <> "}\n"
writeStmt (JsExpr expr) = writeExpr expr <> ";\n"
writeStmt JsContinue = "continue;\n"
writeStmt (JsAssignment to expr) = to <> " = " <> writeExpr expr <> ";\n"
writeStmt (JsWhileTrue stmts) = "while (true) {\n" <> concatMap writeStmt stmts <> "}\n"

writeExpr :: JsExpr -> String
writeExpr (JsLit lit) = writeLit lit
writeExpr (JsVar name) = name
writeExpr (JsInline inline) = inline
writeExpr (JsFunc arg expr) = "($" <> show arg <> ") => " <> writeExpr expr
writeExpr (JsBin op l r) = "(" <> writeExpr l <> " " <> op <> " " <> writeExpr r <> ")"
writeExpr (JsNeg expr) = "-(" <> writeExpr expr <> ")"
writeExpr (JsImmediatelyEvaluatedLambda stmts) = "(()=>{\n" <> concatMap writeStmt stmts <> "})()"
writeExpr (JsCall callee arg) = "(" <> writeExpr callee <> ")(" <> writeExpr arg <> ")"

writeLit :: JsLit -> String
writeLit JsUndefined = "undefined"
writeLit JsNull = "null"
writeLit (JsBool t) = if t then "true" else "false"
writeLit (JsInt i) = show i
writeLit (JsDouble d) = show d
writeLit (JsString str) = '"' : str <> "\""

writeStmts :: [JsStmt] -> String
writeStmts = concatMap ((<> "\n") . writeStmt)
