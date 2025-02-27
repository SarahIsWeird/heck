module Heck.Stuff where

import Language.Haskell.Exts (Decl, ImportDecl, ModulePragma, SrcSpanInfo)

data HsModule = HsModule
  { hsmLoc :: SrcSpanInfo,
    hsmModuleName :: Maybe String,
    hsmModulePragmas :: [ModulePragma SrcSpanInfo],
    hsmImportDecls :: [ImportDecl SrcSpanInfo],
    hsmDecls :: [Decl SrcSpanInfo]
  }
