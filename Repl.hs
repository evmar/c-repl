-- c-repl: a C read-eval-print loop.
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

import Prelude hiding (log)
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.List (find, isPrefixOf, intercalate, stripPrefix)
import Data.Maybe (catMaybes, mapMaybe)
import qualified System.Console.Readline as Readline
import System.Environment
import System.Exit
import System.IO
import System.Posix.Types (ProcessID)
import System.Process
import System.FilePath
import System.Directory

import qualified Child
import qualified CodeSnippet
import CodeSnippet (CodeSnippet)
import qualified GCCXML
import qualified GDBMI

log :: Show a => String -> a -> IO ()
log desc obj = putStrLn (desc ++ " " ++ show obj)

data InterpEnv = InterpEnv {
  envVerbose :: Bool,         -- Verbose flag.
  envChild   :: Child.Child,  -- Child process that executes code.
  envHeaders :: [String],     -- Headers to #include, like "<stdio.h>".
  envLibraries :: [String],   -- Libraries to link in, like "foo" in -l"foo".
  envSyms    :: [(String, GCCXML.Symbol)],  -- Imported header symbols.
  envDecls   :: [String],     -- Declared variables.
  envEntry   :: Int           -- Current .so number we're on.
}
instance Show InterpEnv where
  show env = "headers: " ++ show (envHeaders env)
          ++ " decls: " ++ show (envDecls env)
          ++ " entry: " ++ show (envEntry env)

creplDir = ".c-repl"
cleanupDir :: IO ()
cleanupDir = do
  exists <- doesDirectoryExist creplDir
  when exists $ do
    files <- getDirectoryContents creplDir
    sequence_ [removeFile (creplDir </> x) | x <- files, x /= "." && x /= ".."]
    removeDirectory creplDir

setupDir = do
  cleanupDir
  createDirectory creplDir

includesAsSource :: [String] -> String
includesAsSource = concatMap (\h -> "#include " ++ h ++ "\n")

makeSnippet :: InterpEnv -> String -> Int -> Either String (InterpEnv, String)
makeSnippet env code entry = do
  snippet <- CodeSnippet.parse code
  let source = snippetToSource env snippet entry
  let decl = snippetToDecl snippet
  return (env {envDecls=envDecls env ++ catMaybes [decl]},
          source)

snippetToDecl :: CodeSnippet -> Maybe String
snippetToDecl (CodeSnippet.Code _)         = Nothing
snippetToDecl (CodeSnippet.VarDecl decl _) = return decl
snippetToDecl (CodeSnippet.FunDecl decl _) = return decl

snippetToSource :: InterpEnv -> CodeSnippet -> Int -> String
snippetToSource env snippet entry =
  intercalate "\n" [incl, decls, line, global snippet,
                    func, line, local snippet, "}\n"]
  where
  incl = includesAsSource (envHeaders env)
  decls = concatMap (++ ";\n") (envDecls env)
  line = "#line 1"  -- So gcc error messages have user-understandable lineno.
  global (CodeSnippet.Code _) = ""
  global (CodeSnippet.VarDecl decl _) = decl ++ ";"
  global (CodeSnippet.FunDecl decl code) = decl ++ code ++ ";"
  func = "void dl" ++ show entry ++ "() {"
  local (CodeSnippet.Code str) = str ++ ";"
  local (CodeSnippet.VarDecl _ str) = str ++ ";"
  local (CodeSnippet.FunDecl _ _ ) = ""

generateSharedObject :: InterpEnv -> String -> IO (Either String ())
generateSharedObject env snippet = do
  let libs = concatMap (\lib -> "-l" ++ lib ++ " ") (envLibraries env)
  let soname = creplDir </> "dl" ++ show (envEntry env) ++ ".so"
  let cmd = "gcc -Wall " ++ libs ++ "-xc -g -shared -fPIC -o " ++ soname ++ " -"
  (inp,out,err,pid) <- runInteractiveCommand cmd 
  error <- hGetContents err
  hPutStr inp snippet
  hClose inp
  exit <- waitForProcess pid
  when (not (null error)) $ putStr error
  case exit of
    ExitSuccess -> return (return ())
    ExitFailure code -> return (throwError "compile failed.")

-- c-repl meta-level parse of a line.
data Command = IncludeHeader String
             | Code String
             | TypeQuery String
             | InfoQuery String
             | LoadLibrary String
             | HelpQuery

metacommands :: [(String, String, String -> Command)]
metacommands = [
  ("t", "print the type of a symbol",    TypeQuery),
  ("p", "print the value of a variable", InfoQuery),
  ("i", "#include a header",             IncludeHeader),
  ("l", "load a library",                LoadLibrary)
  ]

parseLine :: String -> Either String Command
parseLine line | inc `isPrefixOf` line =
  let Just h = stripPrefix inc line in return $ IncludeHeader h
  where inc = "#include "
parseLine ('.':line) =
  let (cmd, rest) = breakApart (==' ') line in
  case find (\(key,_,_) -> key `isPrefixOf` cmd) allcommands of
    Just (_, _, command) -> return (command rest)
    Nothing -> throwError "unknown command"
  where
  allcommands = ("h", "", const HelpQuery) : metacommands
  breakApart pred l =
    let (a,b) = break pred l in
    case b of [] -> (a, b); _ -> (a, tail b)
parseLine line = return $ Code line

runLine :: InterpEnv -> String -> IO (Either String InterpEnv)
runLine env line = runErrorT $ do
  cmd <- ErrorT $ return $ parseLine line
  case cmd of
    IncludeHeader header -> do
      let env' = env {envHeaders=envHeaders env ++ [header]}
      updateCompletionSymbols env'
    Code code -> do
      let entry = envEntry env
      (env', code) <- ErrorT $ return (makeSnippet env line entry)
      runCode env' code
    TypeQuery var -> do
      liftIO $ case lookup var (envSyms env) of
        Nothing -> putStrLn "unknown"
        Just sym -> putStrLn $ GCCXML.showSymbol sym
      return env
    LoadLibrary lib -> do
      return $ env {envLibraries=envLibraries env ++ [lib]}
    InfoQuery var -> do
      let pid = fromIntegral (Child.childPid (envChild env))
      let cmd = GDBMI.MICommand ("var-create v * " ++ var)
      GDBMI.MIOutput log out <- ErrorT $ runGDB pid cmd
      case out of
        Nothing -> throwError $ "GDB unexpected output " ++ show log
        Just (GDBMI.MIError e) -> throwError $ "GDB error: " ++ show e
        Just (GDBMI.MIDone args) -> do
          vals <- return $ mapM (`lookup` args) ["type", "value"]
          case vals of
            Just [GDBMI.MIString typ, GDBMI.MIString val] ->
              liftIO $ putStrLn $ typ ++ ": " ++ val
            _ -> throwError $ "bad output args: " ++ show args
      return env
    HelpQuery -> do
      liftIO $ do
        putStrLn "you can enter:"
        putStrLn "- snippets of code: e.g. int x = 3 or printf(\"hi\\n\")"
        putStrLn "- includes: e.g. #include <foobar.h>"
        putStrLn "- or a metacommand of the form '.command args'"
        putStrLn "metacommands are:"
        forM_ metacommands $ \(key, desc, _) ->
          putStrLn $ "- " ++ key ++ ": " ++ desc
      return env
  where
  runCode :: InterpEnv -> String -> ErrorT String IO InterpEnv
  runCode env code = do
    let entry = envEntry env
    liftIO $ when (envVerbose env) $ putStrLn code
    ErrorT $ generateSharedObject env code
    runok <- liftIO $ Child.run (envChild env) entry
    case runok of
      Left err -> do
        -- Run failed.  Reboot the child.
        liftIO $ putStrLn err
        child <- ErrorT Child.start
        return $ env {envChild=child, envEntry=1}
      Right ok -> return $ env {envEntry = envEntry env + 1}

runGDB :: ProcessID -> GDBMI.GDBCommand -> IO (Either String GDBMI.MIOutput)
runGDB pid cmd = bracket before after todo where
  before = GDBMI.attach Nothing pid
  after :: Either String (GDBMI.GDB, GDBMI.MIOutput) -> IO ()
  after (Right (gdb, log)) = GDBMI.detach gdb
  after _                  = return ()
  todo (Right (gdb, log)) = GDBMI.runCommand cmd gdb
  todo (Left err)         = return $ throwError err

updateCompletionSymbols :: InterpEnv -> ErrorT String IO InterpEnv
updateCompletionSymbols env = do
  let code = includesAsSource (envHeaders env)
  symbols <- ErrorT $ GCCXML.symbols code
  let newsyms = mapMaybe (\sym -> do name <- symbolName sym; return (name, sym))
                         symbols
  let names = map fst newsyms
  when (envVerbose env) $ liftIO $ print names
  liftIO $ Readline.setCompletionEntryFunction (Just (complete names))
  return $ env {envSyms=envSyms env ++ newsyms}
  where
  complete names input = return $ filter (input `isPrefixOf`) names
  symbolName (GCCXML.Function name args) = return name
  symbolName (GCCXML.Type _) = Nothing

main = do
  args <- getArgs
  let verbose = case args of
                  "-v":_ -> True
                  _ -> False
  putStrLn "c-repl: a C read-eval-print loop."
  putStrLn "enter '.h' at the prompt for help."
  -- Turn off the space after tab-completion.
  Readline.setCompletionAppendCharacter (Just '\0')
  bracket_ setupDir cleanupDir $ do
    env <- runErrorT $ do
      child <- ErrorT Child.start
      let env = InterpEnv {
        envVerbose=verbose, envChild=child,
        envHeaders=["<stdio.h>", "<math.h>"], envLibraries=["m"],
        envSyms=[], envDecls=[],
        envEntry=1}
      updateCompletionSymbols env
    case env of
      Left error -> putStrLn $ "error: " ++ error
      Right env ->
        loop env `finally` (Child.stop (envChild env))
    where
    loop env = do
      line <- Readline.readline "> "
      case line of
        Nothing   -> putStrLn "" >> return ()  -- EOF; time to die.
        Just line -> do
          Readline.addHistory line
          env' <- runLine env line
          case env' of
            Left err -> do putStrLn err; loop env
            Right env' -> loop env'
