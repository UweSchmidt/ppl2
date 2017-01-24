{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- haskell-mode wants this

module PPL2.CodeGen.Monad
  ( GenCode
  , GCError(..)
  , runGC
  , abortGC
  , errGCExpr
  , newLabel
  , newGlobals
  , newGlobVal
  , addFctCode
  , fctCode
  , globSeg
  )
where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.Types
import PPL2.CodeGen.Builder

-- ----------------------------------------

import Control.Applicative
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError
                            ,MonadIO, liftIO)
import Control.Monad.State  (State(..), MonadState, runState)

-- ----------------------------------------

newtype GenCode v a
  = GC { unGC :: ExceptT (GCError v) (State (GCState v)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (GCState v)
           , MonadError (GCError v)
           )

runGC :: GenCode v a -> (Either (GCError v) a, GCState v)
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

data GCState v
  = GCS { _labCnt     :: Int
        , _globSegCnt :: Offset
        , _globSeg    :: Builder v
        , _fctCode    :: Code
        }
--    deriving Show

newGCState :: GCState v
newGCState =
  GCS { _labCnt     = 1
      , _globSegCnt = 0
      , _globSeg    = mempty
      , _fctCode    = mempty
      }

labCnt :: Lens' (GCState v) Int
labCnt k s = (\ new -> s {_labCnt = new}) <$> k (_labCnt s)

globSegCnt :: Lens' (GCState v) Offset
globSegCnt k s = (\ new -> s {_globSegCnt = new}) <$> k (_globSegCnt s)

globSeg :: Lens' (GCState v) (Builder v)
globSeg k s = (\ new -> s {_globSeg = new}) <$> k (_globSeg s)

fctCode :: Lens' (GCState v) Code
fctCode k s = (\ new -> s {_fctCode = new}) <$> k (_fctCode s)

newLabel :: GenCode v Label
newLabel = do
  i <- labCnt <+= 1
  return $ 'l' : show i

newGlobals :: Offset -> v -> GenCode v ()
newGlobals len def = do
  globSeg %= (<> BU (replicate (fromIntegral len) def ++))
  globSegCnt += len

newGlobVal :: v -> GenCode v Address
newGlobVal v = do
  globSeg %= (<> BU (v :))
  AbsA <$> (globSegCnt <+= 1)

addFctCode :: Code -> GenCode v ()
addFctCode is =
  fctCode %= (<> is)

-- ----------------------------------------
