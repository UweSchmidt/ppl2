module PPL2.VM.Control.Instructions where

import PPL2.Prelude
import PPL2.VM.Types

import qualified PPL2.VM.Memory.RTS       as RTS
import qualified PPL2.VM.Memory.Segment   as Segment
import           PPL2.VM.Memory.State     (MStatus(..), msFrames)

import           PPL2.VM.Control.MicroOps
import           PPL2.VM.Control.Types    (MicroInstr, abort)

import           PPL2.VM.ALU.Types        (ALU, getMicroInstr)

-- ----------------------------------------
--
-- memory access

iLoad :: DataRefValue v => Address -> MicroInstr v
iLoad a = getByAddress a >>= push

iStore :: DataRefValue v => Address -> MicroInstr v
iStore a = pop >>= putByAddress a

iLoadInd :: DataRefValue v => MicroInstr v
iLoadInd = pop >>= toDataRef >>= getByDataRef >>= push

iStoreInd :: DataRefValue v => MicroInstr v
iStoreInd = do
  v <- pop >>= toDataRef     -- get the address
  pop >>= putByDataRef v     -- get the value and store it

iLoadI :: WordValue v => Int -> MicroInstr v
iLoadI i = push $ _Int # i

iLoadAddr :: DataRefValue v => Address -> MicroInstr v
iLoadAddr a =
  ((_DataRef #) <$> address2ref' a) >>= push

-- ----------------------------------------
--
-- evaluation stack manipulation

iPop :: MicroInstr v
iPop = void pop

iDup :: Offset -> MicroInstr v
iDup i = do
  v <- pop
  push v
  push v

iSwap :: MicroInstr v
iSwap = do
  v1 <- pop
  v2 <- pop
  push v1
  push v2

-- ----------------------------------------
--
-- control flow

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
  i <- (+ fromIntegral (disp - 1)) <$> getPC
  getInstr i >> push (_CodeRef # i)

iJumpInd :: CodeRefValue v => MicroInstr v
iJumpInd = pop >>= toCodeRef >>= setPC

iSRJumpInd :: CodeRefValue v => MicroInstr v
iSRJumpInd = do
  i <- pop >>= toCodeRef
  (review _CodeRef <$> getPC) >>= push
  setPC i

-- ----------------------------------------
--
-- runtime stack manipulation

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

-- ----------------------------------------
--
-- the final operation

iTerm :: MicroInstr v
iTerm = abort Terminated

-- ----------------------------------------
--
-- labels shouldn't occur in machine code,
-- only in assembler, but are still in the Instr set
-- so labels are interpreted as noop

iLabel :: MicroInstr v
iLabel = return ()

-- ----------------------------------------
--
-- the working horse
-- all compute power is in the ALU
--
-- and the ALU is plugged in in the execution loop

iComp :: ALU v -> OpCode -> MicroInstr v
iComp alu oc =
  join $ check' IllegalOpCode (getMicroInstr oc alu)

-- ----------------------------------------
