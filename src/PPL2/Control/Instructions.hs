module PPL2.Control.Instructions where

import           PPL2.Prim.Prelude
import           PPL2.Prim.Values
import           PPL2.Prim.Instr

import qualified PPL2.Memory.RTS     as RTS
import qualified PPL2.Memory.Segment as Segment
import           PPL2.Memory.State

import           PPL2.Control.MicroOperations
import           PPL2.Control.Monad
import           PPL2.ALU.Types

-- ----------------------------------------

iLoad :: DataRefValue v => Address -> MicroInstr v
iLoad a = getByAddress a >>= push

iStore :: DataRefValue v => Address -> MicroInstr v
iStore a = pop >>= putByAddress a

iLoadInd :: DataRefValue v => MicroInstr v
iLoadInd = pop >>= toDataRef >>= getByDataRef >>= push

iStoreInd :: DataRefValue v => MicroInstr v
iStoreInd = do
  v <- pop
  pop >>= toDataRef >>= flip putByDataRef v

iLoadI :: WordValue v => Int -> MicroInstr v
iLoadI i = push $ _Int # i

iPop :: MicroInstr v
iPop = void pop

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

iLoadAddr :: DataRefValue v => Address -> MicroInstr v
iLoadAddr a =
  ((_DataRef #) <$> address2ref' a) >>= push

iBr :: WordValue v => Bool -> Displ -> MicroInstr v
iBr b disp = do
  v <- pop >>= toValue _Bool
  when (b == v) $
    modPC (disp - 1)  -- pc already incremented

iJump :: CodeRefValue v => Displ -> MicroInstr v
iJump disp = modPC (disp - 1)

iSRJump :: CodeRefValue v => Displ -> MicroInstr v
iSRJump disp = do
  ((_CodeRef #) <$> getPC) >>= push
  modPC (disp - 1)

iLoadLab :: CodeRefValue v => Displ -> MicroInstr v
iLoadLab disp = do
  i <- (+ toEnum (disp - 1)) <$> getPC
  getInstr i >> push (_CodeRef # i)

iJumpInd :: CodeRefValue v => MicroInstr v
iJumpInd = pop >>= toCodeRef >>= setPC

iSRJumpInd :: CodeRefValue v => MicroInstr v
iSRJumpInd = do
  i <- pop >>= toCodeRef
  (review _CodeRef <$> getPC) >>= push
  setPC i

iEnter :: DefaultValue v => Offset -> MicroInstr v
iEnter ub =
  msFrames %= RTS.push newFrame
  where
    newFrame = Segment.new ub (_Default # ())

iLeave :: MicroInstr v
iLeave =
  (RTS.pop <$> use msFrames)
    >>= check' RTStackUnderflow
    >>= (msFrames .=)

iTerm :: MicroInstr v
iTerm = abort Terminated

iLabel :: MicroInstr v
iLabel = return ()

-- ----------------------------------------
--
-- the working horse

iComp :: ALU v -> OpCode -> MicroInstr v
iComp alu oc =
  join $ check' IllegalOpCode (getMicroInstr oc alu)

-- ----------------------------------------
