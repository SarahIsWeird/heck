module Heck.Parsing
  ( validatePragmas,
    validateImports,
    runValidations,
    parse,
  )
where

import Control.Monad (forM_)
import Data.Maybe (isJust, mapMaybe)
import Heck.Stuff
import Heck.Utils
import Language.Haskell.Exts hiding (loc, name, parse)

exitInvalidModule :: IO a
exitInvalidModule = do
  errorExit "XML modules aren't supported. Why exactly do these exist?"

parse :: FilePath -> IO HsModule
parse path = do
  res <- parseFile path
  parsedMod <- unwrapParseResult res
  hsMod <- maybe exitInvalidModule return (makeHsModule parsedMod)
  forM_ (runValidations [validatePragmas, validateImports] hsMod) errorExit
  return hsMod

printFailure :: SrcLoc -> String -> IO a
printFailure loc msg = do
  putStrLn $ "Error in " ++ srcFilename loc ++ " line " ++ show (srcLine loc) ++ " column " ++ show (srcColumn loc)
  errorExit msg

getModuleName :: ModuleHead SrcSpanInfo -> String
getModuleName (ModuleHead _ (ModuleName _ name) _ _) = name

unwrapParseResult :: ParseResult a -> IO a
unwrapParseResult (ParseOk a) = return a
unwrapParseResult (ParseFailed loc msg) = do
  printFailure loc msg

makeHsModule :: Module SrcSpanInfo -> Maybe HsModule
makeHsModule (Module l h p i d) =
  Just $ HsModule l (fmap getModuleName h) p i d
makeHsModule _ = Nothing

validatePragmas :: HsModule -> [String]
validatePragmas m = mapMaybe validatePragma (hsmModulePragmas m)

validatePragma :: ModulePragma SrcSpanInfo -> Maybe String
validatePragma (AnnModulePragma l _) =
  Just $ "Annotation pragmas aren't supported." ++ prettyPrint (srcInfoSpan l)
validatePragma (LanguagePragma l _) =
  Just $ "Language pragmas aren't supported." ++ prettyPrint (srcInfoSpan l)
validatePragma OptionsPragma {} = Nothing

validateImports :: HsModule -> [String]
validateImports m = mapMaybe validateImport (hsmImportDecls m)

validateImport :: ImportDecl SrcSpanInfo -> Maybe String
validateImport decl
  | importQualified decl = Just "Sorry, qualified imports don't exist yet."
  | importSrc decl = Just "Source imports aren't a thing yet."
  | importSafe decl = Just "Safe imports aren't a thing yet."
  | isJust (importAs decl) = Just "Import aliases aren't a thing yet."
  | isJust (importSpecs decl) = Just "Import specs aren't a thing yet."
  | otherwise = Nothing

joinWithNewline :: [String] -> String
joinWithNewline = concat . concatMap (\s -> [s, "\n"])

runValidations :: [HsModule -> [String]] -> HsModule -> Maybe String
runValidations [] _ = Nothing
runValidations (v : vs) m =
  case v m of
    ls@(_ : _) -> Just (joinWithNewline ls)
    [] -> runValidations vs m
