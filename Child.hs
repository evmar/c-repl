-- c-repl: a C read-eval-print loop.
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

module Child where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import Data.Maybe
import System.Directory
import System.Exit
import System.Process
import System.IO
import System.Posix.IO (createPipe, fdToHandle)

import qualified Paths_c_repl

-- TODO: rewrite this to not use runProcess, as we want the real pid of
-- the child process (for attaching to it with gdb), and System.Process
-- only exposes ProcessHandles and no pids.

data Child = Child {
  childPHandle  :: ProcessHandle,
  childPid      :: Int,  -- The actual process ID of this process.
  childCommand  :: Handle,
  childResponse :: Handle
}

-- Compute the location of the child helper.
findChildBinary :: IO (Maybe FilePath)
findChildBinary = do
  let path = "dist/build/c-repl-child"
  ok1 <- isReadable path
  if ok1
    then return (Just path)
    else do
      libexecdir <- Paths_c_repl.getLibexecDir
      let path = libexecdir ++ "/c-repl-child"
      ok2 <- isReadable path
      if ok2
        then return (Just path)
        else return Nothing
  where
  isReadable path =
    do
      perms <- getPermissions path
      return $ readable perms
    `catch` \e -> return False

-- Create a new Child, starting the helper process.
start :: IO (Either String Child)
start = do
  (commandR,  commandW)  <- createPipe
  (responseR, responseW) <- createPipe
  childPath <- findChildBinary
  case childPath of
    Nothing -> return $ throwError "couldn't find helper binary"
    Just childPath -> do
      phandle <- runProcess childPath
                     [show commandR, show responseW]
                     Nothing{-working dir-} Nothing{-env-}
                     Nothing Nothing Nothing {-stdin,out,err-}
      [commandH, responseH] <- mapM fdToHandle [commandW, responseR]
      mapM_ (\h -> hSetBuffering h LineBuffering) [commandH, responseH]
      pidstr <- hGetLine responseH
      return $ Right $ Child phandle (read pidstr) commandH responseH

-- Kill off a Child.
stop :: Child -> IO ()
stop child = terminateProcess (childPHandle child)

-- Command a Child to run modules up to a given id.
run :: Child -> Int -> IO (Either String ())
run child entry = runErrorT (sendCommand >> awaitResponse) where
  command = show entry
  sendCommand = liftIO $ hPutStrLn (childCommand child) command
  awaitResponse :: ErrorT String IO ()
  awaitResponse = do
    -- Set up a thread that fills in a MVar if the child responds.
    respMVar <- liftIO $ do
      respMVar <- newEmptyMVar
      forkIO $ do
        resp <- hGetLine (childResponse child)
        putMVar respMVar resp
      return respMVar
    -- Wait up to 5s for a response.
    resp <- checkResponse respMVar 5000
    -- Check that the response is as we expect.
    if resp == command
      then return ()
      else throwError "got bad response from child"

  checkResponse :: MVar String -> Int -> ErrorT String IO String
  checkResponse respMVar ms = do
    resp <- liftIO $ tryTakeMVar respMVar
    case resp of
      Just resp -> return resp
      Nothing -> do -- still working?
        -- The subprocess hasn't responded yet.  Check if it died.
        -- (Sometimes getProcessExitCode throws an interrupted exception;
        -- we interpret that as a crash as well.)
        dead <- liftIO $ isDead child
        if dead
          then throwError "(child exited)"
          else if ms <= 0
            then do
              -- We've waited too long.  (XXX prompt the user here)
              liftIO $ terminateProcess (childPHandle child)
              throwError "(child hung?)"
            else do
              -- Wait a bit longer for a response.
              liftIO $ threadDelay 100
              checkResponse respMVar (ms-100)

isDead :: Child -> IO Bool
isDead child = catchJust ioErrors getExited (\e -> return True) where
  getExited = do
    exit <- getProcessExitCode (childPHandle child)
    return $ isJust exit
