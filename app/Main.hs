module Main where

import Data.Maybe (fromMaybe)
import Heck
import Heck.CodeWriter
import Heck.Compiler (compileModule, runCompiler)

printModuleHead :: HsModule -> IO ()
printModuleHead m =
  putStrLn $ fromMaybe "<anonymous>" (hsmModuleName m)

main :: IO ()
main = do
  hsMod <- parse "./examples/Hello.hs"
  let stmts = runCompiler (compileModule hsMod)
  printModuleHead hsMod
  putStrLn "========="
  putStrLn (writeStmts stmts)
  return ()
