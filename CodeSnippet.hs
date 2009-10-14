-- c-repl: a C read-eval-print loop.
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- This module parses REPL inputs.  We need to parse a
-- declaration like "int x = foo()" because we compile that into a
-- global declaration of x along with a call to an initializer.
-- The code is pretty hacky but it passes the (inline) test suite.

module CodeSnippet (
  -- Parsed snippet of code, to the level of parsing we care about.
  CodeSnippet(..),
  -- Parse an input into a CodeSnippet.
  parse,
  -- Expose the test runner so we can use it via ghci.
  runTests
) where

import Control.Monad.Error
import Data.Char
import Data.List
import Test.HUnit
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec

data CodeSnippet = Code String
                 | VarDecl String String  -- Decl, initialization code.
                 | FunDecl String String  -- Type + name, body.
                 deriving (Eq,Show)

type TokenStream = [(SourcePos, Token)]
tokPos = fst
data Token = Ident String | Punct String deriving (Eq, Show)

substr :: Maybe SourcePos -> Maybe SourcePos -> String -> String
substr start end str = strip $ take sublen $ drop startOfs $ str
  where
  startOfs = maybe 0 spOfs start
  endOfs = maybe (length str) spOfs end
  sublen = endOfs - startOfs
  spOfs sp = sourceColumn sp - 1
  strip []     = []
  strip [' ']  = []
  strip (x:xs) = x : strip xs
stripSemi []     = []
stripSemi [';']  = []
stripSemi (x:xs) = x : stripSemi xs

parse :: String -> Either String CodeSnippet
parse input = do
  -- Properly parsing C is famously impossible without processing typedefs in
  -- all headers.  But we can get pretty close with some heuristics.
  -- This code is hideous, but it sorta comes with the territory.
  case Parsec.parse p_tokenize "code" input of
    Left err -> Left (show err)
    Right tokenstream -> do
      let (idents, rest) = span (isTypeLeader . snd) tokenstream
      if length idents < 2
        then return $ Code (stripSemi input)
        else let (typ, var) = (init idents, last idents)
             in parseDecl typ var rest
  where
  parseDecl typ var ((npos, Punct "("):rest) =
    case dropWhile (\(_,tok) -> tok /= Punct ")") rest of
      (rparen:(next,_):rest) ->
        return $ FunDecl (substr Nothing (Just next) input)
                         (substr (Just next) Nothing input)
      _ -> Left $ "couldn't find rparen"
  parseDecl typ var rest =
    let nextpos = case rest of
                    ((pos, tok):rest) | tok /= Punct ";" -> Just pos
                    _ -> Nothing
        code = case nextpos of
                 Just n  -> substr (Just (tokPos var)) Nothing input
                 Nothing -> ""
    in return $ VarDecl (stripSemi $ substr Nothing nextpos input)
                        (stripSemi code)
  isTypeLeader (Ident _)   = True
  isTypeLeader (Punct "*") = True
  isTypeLeader _ = False

p_tokenize :: Parser TokenStream
p_tokenize = many (annotate p_ident <|> annotate p_token) where
  p_ident = liftM Ident $ withSpaces $ many1 (letter <|> digit <|> char '_')
  p_token = do l <- withSpaces $ oneOf "()*[]={};"; return $ Punct [l]
  withSpaces p = do r <- p; skipMany space; return r
  annotate p = do
    pos <- getPosition
    p' <- p
    return (pos, p')

assertParse :: CodeSnippet -> String -> Assertion
assertParse expected input = do
  case parse input of
    Left error -> assertFailure $ show input ++ "  failed to parse: " ++ error
    Right snip -> assertEqual input expected snip

testParse exp input = test $ assertParse exp input

runTests =
  runTestTT $ test $ TestList [
    testParse (VarDecl "int x" "x = 3") "int x = 3;"
  , testParse (VarDecl "int x" "x = 3") "int x = 3"
  , testParse (VarDecl "int xx" "xx = 3") "int xx = 3;"
  , testParse (Code "x = 3") "x = 3"
  , testParse (Code "*((char*)x) = 0") "*((char*)x) = 0;"
  , testParse (VarDecl "int x" "") "int x"
  , testParse (VarDecl "const char* x" "") "const char* x;"
  , testParse (Code "x+y = 4") "x+y = 4;"
  , testParse (Code "for (;;) x") "for (;;) x;"
  , testParse (FunDecl "void f()" "{}") "void f() {}"
  ]

main = runTests
