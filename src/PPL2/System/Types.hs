module PPL2.System.Types
  ( module PPL2.System.Types
  , module System.Exit
  , module System.IO
  )
where

import PPL2.Prelude

import System.Exit
import System.IO

-- the control monad for control flow
-- in main application

type RunCompile a   = ExceptT ExitCode IO a

type MonadCompile m = (MonadIO m, MonadError ExitCode m)

execCompile :: RunCompile () -> IO ()
execCompile cmd =
  runExceptT cmd >>= either exitWith (const exitSuccess)

issueError :: MonadCompile m => Int -> String -> m a
issueError rc ms = do
  liftIO $ hPutStrLn stderr ms
  throwError (ExitFailure rc)

tostdout :: MonadCompile m => String -> m ()
tostdout = liftIO . putStrLn

tostderr :: MonadCompile m => String -> m ()
tostderr = liftIO . hPutStrLn stderr

-- ----------------------------------------
