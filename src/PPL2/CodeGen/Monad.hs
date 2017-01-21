{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- haskell-mode wants this

module PPL2.CodeGen.Monad
  ( GenCode
  , GCError
  , runGC
  , abortGC
  , errGCExpr
  , newLabel
  )
where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.Types

-- ----------------------------------------

import Control.Applicative
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError
                            ,MonadIO, liftIO)
import Control.Monad.State  (State(..), MonadState, runState)

-- ----------------------------------------

newtype GenCode v a
  = GC { unGC :: ExceptT (GCError v) (State GCState) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GCState
           , MonadError (GCError v)
           )

runGC :: GenCode v a -> (Either (GCError v) a, GCState)
runGC = flip (runState . runExceptT . unGC) newGCState

abortGC :: GCError v -> GenCode v a
abortGC = throwError

-- ----------------------------------------

data GCError v
  = GCE String (Maybe (Expr v))
    deriving Show

errGCExpr :: Expr v -> GCError v
errGCExpr = GCE "can't gencode" . Just

-- ----------------------------------------

data GCState
  = GCS { _labCnt :: Int
        }
    deriving Show

newGCState :: GCState
newGCState =
  GCS { _labCnt = 1
      }

labCnt :: Lens' GCState Int
labCnt k s = (\ new -> s {_labCnt = new}) <$> k (_labCnt s)

newLabel :: GenCode v Label
newLabel = do
  i <- labCnt <+= 1
  return $ 'L' : show i

-- ----------------------------------------
