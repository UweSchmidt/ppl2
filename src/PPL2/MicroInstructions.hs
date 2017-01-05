{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PPL2.MicroInstructions where

import           PPL2.Prim.Prelude
import           PPL2.Prim.MValue
import           PPL2.Prim.MInstr

import           PPL2.Memory.RTS     (MVRTS)
import qualified PPL2.Memory.RTS     as RTS
import           PPL2.Memory.Segment (Segment, MVSegment)
import qualified PPL2.Memory.Segment as Segment
import           PPL2.Memory.Stack   (MVStack)
import qualified PPL2.Memory.Stack   as Stack
import           PPL2.Memory.CodeSeg (CodeSegment)
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

data MState v     = MS { instr  :: ! CodeSegment
                       , pc     :: ! Word
                       , stack  :: ! (EvalStack v)
                       , mem    :: ! (MSeg v)
                       , frames :: ! (RTStack v)
                       , status :: ! MStatus
                       }

type EvalStack v  = MVStack v
type MSeg      v  = MVSegment v
type RTStack   v  = MVRTS v

data MStatus      = Ok
                  | AddressViolation Address
                  | DataRefViolation DataRef
                  | EvalStackUnderflow
                  | IllegalArgument
                  | PCoutOfRange
                  | IOError String

-- ----------------------------------------

newtype MicroCode v a
  = RT { unRT :: ExceptT () (StateT (MState v) IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadState (MState v)
           , MonadError ()
           , MonadIO
           )

runMicroCode :: MicroCode v a -> MState v -> IO (Either () a, MState v)
runMicroCode m st
  = (runStateT . runExceptT . unRT $ m) st

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

abort :: MStatus -> MicroCode v a
abort exc = do
  msStatus .= exc
  throwError ()

check :: MStatus -> MicroCode v (Maybe a) -> MicroCode v a
check exc cmd =
  cmd >>= maybe (abort exc) return

-- ----------------------------------------

msInstr :: Lens' (MState v) CodeSegment
msInstr k ms = (\ new -> ms {instr = new}) <$> k (instr ms)

msPC :: Lens' (MState v) Offset
msPC k ms = (\ new -> ms {pc = new}) <$> k (pc ms)

msMem :: Lens' (MState v) (MSeg v)
msMem k ms = (\ new -> ms {mem = new}) <$> k (mem ms)

msStack :: Lens' (MState v) (EvalStack v)
msStack k ms = (\ new -> ms {stack = new}) <$> k (stack ms)

msFrames :: Lens' (MState v) (RTStack v)
msFrames k ms = (\ new -> ms {frames = new}) <$> k (frames ms)

msStatus :: Lens' (MState v) MStatus
msStatus k ms = (\ new -> ms {status = new}) <$> k (status ms)

-- ----------------------------------------

getInstr :: MicroCode v MInstr
getInstr = check PCoutOfRange getInstr'

getInstr' :: MicroCode v (Maybe MInstr)
getInstr' = do
  i  <- use msPC
  cs <- use msInstr
  return $ CodeSeg.get i cs

getPC :: MicroCode v Offset
getPC = use msPC

setPC :: Offset -> MicroCode v ()
setPC = (msPC .=)

incrPC :: MicroCode v ()
incrPC = modPC 1

modPC :: Int -> MicroCode v ()
modPC disp = msPC += toEnum disp

-- ----------------------------------------

getMem :: Address -> MicroCode v (MValue v)
getMem a = check (AddressViolation a) $ getMem' a
  where
    getMem' :: Address -> MicroCode v (Maybe (MValue v))
    getMem' (AbsA i)  = Segment.get  i <$> use msMem
    getMem' (LocA i)  = RTS.getLocal i <$> use msFrames

getInd :: DataRef -> MicroCode v (MValue v)
getInd a = check (DataRefViolation a) $ getInd' a
  where
    getInd' :: DataRef -> MicroCode v (Maybe (MValue v))
    getInd' (DR sid i)
      | sid == dataSid = Segment.get  i <$> use msMem
      | otherwise      = RTS.get  sid i <$> use msFrames

-- ----------------------------------------

putMem :: Address -> MValue v -> MicroCode v ()
putMem a@(AbsA i) v = do
  mem' <- check (AddressViolation a)
          (Segment.put i v <$> use msMem)
  msMem .= mem'

putMem a@(LocA i) v = do
  rts' <- check (AddressViolation a)
          (RTS.putLocal i v <$> use msFrames)
  msFrames .= rts'

-- ----------------------------------------

push :: MValue v -> MicroCode v ()
push mv = msStack %= Stack.push mv

pop :: MicroCode v (MValue v)
pop = do
  (v, s')  <- check EvalStackUnderflow $
              uses msStack Stack.get
  msStack .= s'
  return v

-- ----------------------------------------

iLoad :: Address -> MicroCode v ()
iLoad a = getMem a >>= push

iStore :: Address -> MicroCode v ()
iStore a = pop >>= putMem a

-- ----------------------------------------
