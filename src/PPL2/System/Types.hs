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


type MonadCompile m = (MonadIO m, MonadError ExitCode m)

execCompile :: RunCompile () -> IO ()
execCompile cmd =
  runCompile cmd >>= either exitWith (const exitSuccess)

issueError :: MonadCompile m => Int -> String -> m a
issueError rc ms = do
  liftIO $ hPutStrLn stderr ms
  throwError (ExitFailure rc)

tee :: Monad m => (a -> m ()) -> a -> m a
tee out x = out x >> return x

tostdout :: MonadIO m => String -> m ()
tostdout = liftIO . putStrLn

tostderr :: MonadIO m => String -> m ()
tostderr = liftIO . hPutStrLn stderr

todevnull :: Monad m => String -> m ()
todevnull = const $ return ()

stopWhen :: MonadCompile m => m Bool -> a -> m a
stopWhen flag v = do
  b <- flag
  if b
    then tostderr "compile run finished with success"
         >>
         throwError ExitSuccess
    else return v

class Monad m => MonadOptions m where
  onlyAssemble :: m Bool
  onlyGenCode  :: m Bool
  success      :: m Bool

  onlyGenCode  = return False
  onlyAssemble = return False
  success      = return True

-- ----------------------------------------
--
-- the main monad for controling the whole compile process

newtype RunCompile a
  = RC { unRC :: ExceptT ExitCode IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError ExitCode
           , MonadIO
           )

runCompile :: RunCompile a -> IO (Either ExitCode a)
runCompile = runExceptT . unRC

instance MonadOptions RunCompile where

-- ----------------------------------------
