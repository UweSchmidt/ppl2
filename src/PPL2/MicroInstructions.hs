{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PPL2.MicroInstructions where

import           PPL2.Prelude

import           PPL2.Memory.RTS     (RTS)
import qualified PPL2.Memory.RTS     as RTS
import           PPL2.Memory.Segment (Segment)
import qualified PPL2.Memory.Segment as Segment
import           PPL2.Memory.Stack   (Stack)
import qualified PPL2.Memory.Stack   as Stack
import           PPL2.Memory.CodeSeg (CodeSeg)
import qualified PPL2.Memory.CodeSeg as CodeSeg

import Control.Applicative (Applicative(..))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad

import Control.Lens

import qualified Data.Array.IArray as IA

import System.IO

import Control.Exception        ( SomeException
                                , IOException
                                , try
                                )

-- ----------------------------------------

data MState       = MS { instr  :: ! MProg
                       , pc     :: ! Word
                       , stack  :: ! EvalStack
                       , mem    :: ! MSeg
                       , frames :: ! RuntimeStack
                       , status :: ! MStatus
                       }

type MProg        = CodeSeg Instr
type EvalStack    = Stack MValue
type MSeg         = Segment MValue
type RuntimeStack = RTS MValue

data Instr        = Instr -- dummy

data MValue       = MV    -- dummy

data Address      = LocA Offset
                  | AbsA Offset
                  | RefA DataRef

data MStatus      = Ok
                  | AddressViolation Address
                  | PCoutOfRange
                  | IOError String

-- ----------------------------------------

newtype MicroCode a
  = RT { unRT :: ExceptT () (StateT MState IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadState MState
           , MonadError ()
           , MonadIO
           )

runMicroCode :: MicroCode a -> MState -> IO (Either () a, MState)
runMicroCode m st
  = (runStateT . runExceptT . unRT $ m) st

-- ----------------------------------------
--
-- lift IO commands and catch all IO exceptions

io :: IO a -> MicroCode a
io x = do
  r <- liftIO $ try x
  either (abort . IOError . showExc) return $ r
  where
    showExc :: IOException -> String
    showExc = show

abort :: MStatus -> MicroCode a
abort exc = do
  msStatus .= exc
  throwError ()

check :: MStatus -> MicroCode (Maybe a) -> MicroCode a
check exc cmd =
  cmd >>= maybe (abort exc) return

-- ----------------------------------------

msInstr :: Lens' MState MProg
msInstr k ms = (\ new -> ms {instr = new}) <$> k (instr ms)

msPC :: Lens' MState Offset
msPC k ms = (\ new -> ms {pc = new}) <$> k (pc ms)

msMem :: Lens' MState MSeg
msMem k ms = (\ new -> ms {mem = new}) <$> k (mem ms)

msStack :: Lens' MState EvalStack
msStack k ms = (\ new -> ms {stack = new}) <$> k (stack ms)

msFrames :: Lens' MState RuntimeStack
msFrames k ms = (\ new -> ms {frames = new}) <$> k (frames ms)

msStatus :: Lens' MState MStatus
msStatus k ms = (\ new -> ms {status = new}) <$> k (status ms)

-- ----------------------------------------

getInstr :: MicroCode Instr
getInstr = check PCoutOfRange getInstr'

getInstr' :: MicroCode (Maybe Instr)
getInstr' = do
  i  <- use msPC
  cs <- use msInstr
  return $ CodeSeg.get i cs

getPC :: MicroCode Offset
getPC = use msPC

setPC :: Offset -> MicroCode ()
setPC = (msPC .=)

incrPC :: MicroCode ()
incrPC = modPC 1

modPC :: Int -> MicroCode ()
modPC disp = msPC += toEnum disp

-- ----------------------------------------

readMem :: Address -> MicroCode MValue
readMem a =
  check (AddressViolation a) $ readMem' a

readMem' :: Address -> MicroCode (Maybe MValue)
readMem' (AbsA i)  = Segment.get  i <$> use msMem
readMem' (LocA i)  = RTS.getLocal i <$> use msFrames

readMem' (RefA (DR sid i))
  | sid == dataSid = Segment.get  i <$> use msMem
  | otherwise      = RTS.get  sid i <$> use msFrames

-- ----------------------------------------
