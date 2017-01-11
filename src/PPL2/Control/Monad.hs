{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PPL2.Control.Monad where

import PPL2.Prim.Prelude
import PPL2.Memory.State

import Control.Monad.Except
import Control.Monad.State

import Control.Exception (IOException, try)

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
runMicroCode m = (runStateT . runExceptT . unRT $ m)

-- ----------------------------------------
--
-- lift IO commands and catch all IO exceptions

io :: IO a -> MicroCode v a
io x = do
  r <- liftIO $ try x
  either (abort . IOError . showExc) return $ r
  where
    showExc :: IOException -> String
    showExc = show

abort :: MStatus v -> MicroCode v a
abort exc = do
  msStatus .= exc
  throwError ()

-- ----------------------------------------
