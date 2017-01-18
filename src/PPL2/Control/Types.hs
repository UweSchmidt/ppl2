{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- haskell-mode wants this

module PPL2.Control.Types where

import PPL2.Prelude
import PPL2.VM.Types

import PPL2.Memory.State    (MState, MStatus(..), msStatus)

import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError
                            ,MonadIO, liftIO)
import Control.Monad.State  (StateT(..), MonadState)
import Control.Exception    (IOException, try)

-- ----------------------------------------

newtype MicroCode v a
  = RT { unRT :: ExceptT () (StateT (MState v) IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (MState v)
           , MonadError ()
           , MonadIO
           )

type MicroInstr v = MicroCode v ()

runMicroCode :: MicroCode v a -> MState v -> IO (Either () a, MState v)
runMicroCode = runStateT . runExceptT . unRT

-- ----------------------------------------
--
-- lift IO commands and catch all IO exceptions

io :: IO a -> MicroCode v a
io x = do
  r <- liftIO $ try x
  either (abort . IOError . showExc) return r
  where
    showExc :: IOException -> String
    showExc = show

abort :: MStatus v -> MicroCode v a
abort exc = do
  msStatus .= exc
  throwError ()

-- ----------------------------------------
