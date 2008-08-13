-- c-repl: a C read-eval-print loop.
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

module GCCXML (
  symbols,
  Symbol(..),
  showSymbol
) where

import Prelude hiding (catch)
import Control.Monad.Error
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import qualified Data.Map as M
import System.Exit
import System.IO
import System.Process
import Text.XML.Expat.Tree as Expat

type XML = BS.ByteString

-- @runGCCXML code@ runs a gccxml process on |code|, returning the XML output
-- or an error string on error.
runGCCXML :: String -> IO (Either String XML)
runGCCXML code = run `catch` (\e -> do print e; undefined) where
  run = do
    let cmd = "gccxml - -fxml=/dev/stdout"
    (inp,out,err,pid) <- runInteractiveCommand cmd
    hPutStr inp code
    hClose inp
    output <- BS.hGetContents out
    error <- hGetContents err
    exit <- BS.length output `seq` waitForProcess pid
    return $ case exit of
      ExitSuccess      -> return output
      ExitFailure code -> throwError error

-- Symbol resolution: gccxml outputs a DAG as a flat list of nodes with ids
-- and pointers to other nodes.  While parsing, we build a Map of symbol
-- id -> unresolved symbol info, and then once parsing is complete we resolve
-- all references into the real DAG.

-- The identity of a symbol as output by gccxml, such as "_341".
type SymbolId = String
-- A map from symbol id to unresolved symbol.
type SymbolMap = M.Map SymbolId UnrSym
-- An unresolved symbol is either a UnrSym, awaiting a complete SymbolMap,
-- or it's a ResSym, a base case child node like "int".
data UnrSym = UnrSym (SymbolMap -> Either String UnrSym)
            | ResSym Symbol

-- Description of a C-level type.
data CType = Array CType
           | Const CType
           | Enum String
           | Fundamental String
           | CFunction [CType]
           | Pointer CType
           | Struct String
           | Typedef String
           | Union String
           deriving Show

showCType (Array t)       = showCType t ++ "[]"
showCType (Const t)       = showCType t ++ " const"
showCType (Enum t)        = t
showCType (Fundamental t) = t
showCType (CFunction t)   = "[function]"
showCType (Pointer t)     = showCType t ++ "*"
showCType (Struct t)      = t
showCType (Typedef t)     = t
showCType (Union t)       = t

-- The symbols we parse out of gccxml: currently just functions and types.
data Symbol = Function String [String]
            | Type CType
            deriving Show

-- Print a user-friendly version of a Symbol.
showSymbol :: GCCXML.Symbol -> String
showSymbol (Function name args) = name ++ "(" ++ intercalate ", " args ++ ")"
showSymbol (Type typ) = showCType typ

-- Given a symbol map and an unresolved symbol, resolve it to a plain symbol
-- or throw an error.
resolve :: SymbolMap -> UnrSym -> Either String Symbol
resolve map (UnrSym f) = f map >>= resolve map
resolve map (ResSym s) = return s

-- Given a symbol map and an unresolved symbol, resolve it to a type or throw
-- an error.
resolveType :: SymbolMap -> UnrSym -> Either String CType
resolveType map unr = do
  sym <- resolve map unr
  case sym of
    Type ct -> return ct
    x       -> throwError (show x)

-- Convert a plain symbol id to an UnrSym: the unresolved symbol the id
-- references.
symref :: SymbolId -> UnrSym
symref id = UnrSym (\symbolmap ->
  case M.lookup id symbolmap :: Either String UnrSym of
    Left err -> Left $ "lookup failed: " ++ id
    Right ok -> Right ok)

-- The main parser/driver, @parse code@ returns either an error or a list of
-- resolves Symbols.
symbols :: String -> IO (Either String [Symbol])
symbols code = runErrorT $ do
  xml <- ErrorT $ runGCCXML code
  ErrorT $ return $ parseSymbols xml
  where
  parseSymbols :: XML -> Either String [Symbol]
  parseSymbols xml = do
    tree <- case Expat.parse Nothing xml of
              Just (Element root attrs tree) -> return tree
              _ -> throwError "xml parse error"
    let nodes = mapMaybe parseNode tree
    let symbolmap = M.fromList nodes
    mapM (resolve symbolmap . snd) nodes
  parseNode :: Expat.Node -> Maybe (SymbolId, UnrSym)
  parseNode (Element typ attrs kids) = do
    sym <- parseSymbol typ attrs kids
    id <- lookup "id" attrs
    return (id, sym)
  parseNode _ = Nothing

  parseSymbolType0Arg :: [(String,String)] -> (String -> CType) -> Maybe UnrSym
  parseSymbolType0Arg attrs constructor = do
    name <- lookup "name" attrs
    return $ ResSym $ Type $ constructor (prettify name)

  parseSymbolType1Arg :: [(String,String)] -> (CType -> CType) -> Maybe UnrSym
  parseSymbolType1Arg attrs constructor = do
    innertypeid <- lookup "type" attrs
    return $ UnrSym $ \symbolmap -> do
      innertype <- resolveType symbolmap (symref innertypeid)
      return $ ResSym $ Type $ constructor innertype

  parseSymbol :: String -> [(String,String)] -> [Expat.Node] -> Maybe UnrSym
  parseSymbol "Function" attrs kids = do
    name <- lookup "name" attrs
    when (isInternal name) Nothing
    let args = mapMaybe parseFunctionArg kids
    return $ UnrSym $ \symbolmap -> do
      args' <- forM args $ \(unr, name) -> do
        ctype <- resolveType symbolmap unr
        return $ showCType ctype ++ " " ++ name
      return $ ResSym $ Function name args'
  parseSymbol "Union" attrs kids = do
    name <- msum [lookup "name" attrs, lookup "demangled" attrs, Just "anon"]
    return $ ResSym $ Type $ Union (prettify name)
  parseSymbol "Struct" attrs kids = do
    name <- msum [lookup "name" attrs, lookup "demangled" attrs, Just "anon"]
    return $ ResSym $ Type $ Struct (prettify name)
  parseSymbol "FunctionType" attrs kids = do
    return $ ResSym $ Type $ CFunction []
  parseSymbol "Enumeration"     attrs _ = parseSymbolType0Arg attrs Enum
  parseSymbol "FundamentalType" attrs _ = parseSymbolType0Arg attrs Fundamental
  parseSymbol "Typedef"         attrs _ = parseSymbolType0Arg attrs Typedef
  parseSymbol "ArrayType"       attrs _ = parseSymbolType1Arg attrs Array
  parseSymbol "CvQualifiedType" attrs _ = parseSymbolType1Arg attrs Const
  parseSymbol "PointerType"     attrs _ = parseSymbolType1Arg attrs Pointer
  parseSymbol _ _ _ = Nothing

  parseFunctionArg :: Expat.Node -> Maybe (UnrSym, String)
  parseFunctionArg (Element "Argument" attrs _) = do
    name <- lookup "name" attrs
    typeid <- lookup "type" attrs
    return (symref typeid, prettify name)
  parseFunctionArg _ = Nothing

  isInternal :: String -> Bool
  isInternal ('_':_) = True
  isInternal _       = False

  prettify ('_':'_':name) = name
  prettify name           = name
