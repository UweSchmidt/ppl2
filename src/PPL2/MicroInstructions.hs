{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PPL2.MicroInstructions where

import           PPL2.Prim.Prelude
import           PPL2.Prim.MValue
import           PPL2.Prim.Instr

import           PPL2.Memory.RTS     (MVRTS)
import qualified PPL2.Memory.RTS     as RTS
import           PPL2.Memory.Segment (Segment, MVSegment)
import qualified PPL2.Memory.Segment as Segment
import           PPL2.Memory.Stack   (MVStack)
import qualified PPL2.Memory.Stack   as Stack
import           PPL2.Memory.CodeSeg (CodeSegment)
import qualified PPL2.Memory.CodeSeg as CodeSeg

import           PPL2.Pretty.Instr

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
                  | RTStackUnderflow
                  | IllegalArgument
                  | IllegalOpCode
                  | IllegalResult
                  | PCoutOfRange
                  | IOError String
                  | Terminated

statusOk :: MStatus -> Bool
statusOk Ok = True
statusOk _  = False

type ALU v = OpCode -> Maybe (String, (Int, Int), [MValue v] -> MicroCode v [MValue v])

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

type MicroInstr v = MicroCode v ()

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

toDataRef :: MValue v -> MicroCode v DataRef
toDataRef (VDRef r) = return r
toDataRef _         = abort IllegalArgument

toCodeRef :: MValue v -> MicroCode v Offset
toCodeRef (VCRef (CR i)) = return i
toCodeRef _              = abort IllegalArgument

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

setPC :: Offset -> MicroInstr v
setPC = (msPC .=)

incrPC :: MicroInstr v
incrPC = modPC 1

modPC :: Int -> MicroInstr v
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

putMem :: Address -> MValue v -> MicroInstr v
putMem a@(AbsA i) v = do
  mem' <- check (AddressViolation a)
          (Segment.put i v <$> use msMem)
  msMem .= mem'

putMem a@(LocA i) v = do
  rts' <- check (AddressViolation a)
          (RTS.putLocal i v <$> use msFrames)
  msFrames .= rts'

putInd :: DataRef -> MValue v -> MicroInstr v
putInd a@(DR sid i) v
  | sid == dataSid = do
      mem' <- check (DataRefViolation a)
              (Segment.put i v <$> use msMem)
      msMem .= mem'

  | otherwise = do
      rts' <- check (DataRefViolation a)
              (RTS.put sid i v <$> use msFrames)
      msFrames .= rts'


-- ----------------------------------------

push :: MValue v -> MicroInstr v
push mv = msStack %= Stack.push mv

pop :: MicroCode v (MValue v)
pop = do
  (v, s')  <- check EvalStackUnderflow $
              uses msStack Stack.get
  msStack .= s'
  return v

popValue :: Prism' (MValue v) a -> MicroCode v a
popValue p = check IllegalArgument (preview p <$> pop)

popBool :: MicroCode v Bool
popBool = popValue _Bool

popWord :: MicroCode v Word
popWord = popValue _Word

popInt :: MicroCode v Int
popInt = popValue _Int

-- ----------------------------------------

iLoad :: Address -> MicroInstr v
iLoad a = getMem a >>= push

iStore :: Address -> MicroInstr v
iStore a = pop >>= putMem a

iLoadInd :: MicroInstr v
iLoadInd = pop >>= toDataRef >>= getInd >>= push

iStoreInd :: MicroInstr v
iStoreInd = do
  v <- pop
  pop >>= toDataRef >>= flip putInd v

iLoadI :: Int -> MicroInstr v
iLoadI i = push $ _Int # i

iPop :: MicroInstr v
iPop = pop >> return ()

iDup :: MicroInstr v
iDup = do
  v <- pop
  push v
  push v

iSwap :: MicroInstr v
iSwap = do
  v1 <- pop
  v2 <- pop
  push v1
  push v2

iLoadAddr :: Address -> MicroInstr v
iLoadAddr a = check (AddressViolation a) (loadA a) >>= push
  where
    loadA :: Address -> MicroCode v (Maybe (MValue v))
    loadA (LocA i) = (fmap VDRef .     RTS.toDataRef i) <$> use msFrames
    loadA (AbsA i) = (fmap VDRef . Segment.toDataRef i) <$> use msMem

iBr :: Bool -> Displ -> MicroInstr v
iBr b disp = do
  v <- popBool
  when (b == v) $
    modPC (disp - 1)  -- pc already incremented

iJump :: Displ -> MicroInstr v
iJump disp = modPC (disp - 1)

iSRJump :: Displ -> MicroInstr v
iSRJump disp = do
  (VCRef . CR <$> getPC) >>= push
  modPC (disp - 1)

iLoadLab :: Displ -> MicroInstr v
iLoadLab disp = do
  i <- (+ toEnum (disp - 1)) <$> getPC
  check PCoutOfRange
    ((fmap VCRef . CodeSeg.toCodeRef i) <$> use msInstr)
    >>= push

iJumpInd :: MicroInstr v
iJumpInd = pop >>= toCodeRef >>= setPC

iSRJumpInd :: MicroInstr v
iSRJumpInd = do
  i <- pop >>= toCodeRef
  (VCRef . CR <$> getPC) >>= push
  setPC i

iEnter :: Offset -> MicroInstr v
iEnter ub =
  msFrames %= RTS.push newFrame
  where
    newFrame = Segment.new ub VUndef

iLeave :: MicroInstr v
iLeave = do
  rts <- check RTStackUnderflow
         (RTS.pop <$> use msFrames)
  msFrames .= rts

iComp :: ALU v -> OpCode -> MicroInstr v
iComp alu opc = do
  (_name, (noArgs, noRes), evalfct) <- check IllegalOpCode $ return (alu opc)
  args <- getArgs noArgs
  res  <- evalfct args
  when (length res /= noRes) $
    abort IllegalResult
  mapM_ push res
  where
    getArgs 0 = return []
    getArgs n
      | n <= 0 =
          return []
      | otherwise = do
          rs <- getArgs (n -1)
          r  <- pop
          return (r : rs)

iTerm :: MicroInstr v
iTerm = abort Terminated

iLabel :: MicroInstr v
iLabel = return ()

-- ----------------------------------------

instrTrc :: ALU v -> MInstr -> Offset -> MicroInstr v
instrTrc alu ins pc' =
  io $ hPutStrLn stderr line
  where
    line = prettyInstr indent prettyOp prettyJmp prettyLab ins

    indent xs =
      fillLeft 6 (show pc') ++ ": " ++ xs

    prettyOp op' =
      maybe ("not-used-" ++ show op') (^. _1) $ alu op'

    prettyJmp disp =
      [show disp, "--> " ++ show (pc' + toEnum disp)]

    prettyLab disp =
      show disp ++ ":"

-- ----------------------------------------

runCPU :: Bool -> ALU v -> MicroInstr v
runCPU trc alu = go
  where
    go = do
      continue <- statusOk <$> use msStatus
      when continue $ do
        instr <- getInstr

        -- trace the instructions
        when trc $
          getPC >>= instrTrc alu instr

        incrPC
        case instr of
          Load  a    -> iLoad a
          Store a    -> iStore a
          Comp op    -> iComp alu op
          LoadInd    -> iLoadInd
          StoreInd   -> iStoreInd
          LoadI i    -> iLoadI i
          Pop        -> iPop
          Dup        -> iDup
          Swap       -> iSwap
          LoadAddr a -> iLoadAddr a
          Br b    t  -> iBr b t
          Jump    t  -> iJump t
          SRJump  t  -> iSRJump t
          LoadLab t  -> iLoadLab t
          JumpInd    -> iJumpInd
          SRJumpInd  -> iSRJumpInd
          Enter ub   -> iEnter ub
          Leave      -> iLeave
          Term       -> iTerm
          Label _    -> iLabel
        go

initMemory :: [MInstr] -> [MValue v] -> MicroInstr v
initMemory is vs = do
  msInstr .= CodeSeg.new is
  setPC 0
  msMem    .= Segment.newInit vs
  msStack  .= Stack.new
  msFrames .= RTS.new
  msStatus .= Ok

-- ----------------------------------------
