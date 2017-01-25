module PPL2.VM.Memory.State
       ( MState(..)
       , MStatus(..)
       , newMState
       , msInstr, msPC, msMem, msStack, msFrames, msStatus
       , statusOk, statusTerminated
       )
where

import PPL2.Prelude
import PPL2.VM.Types

import PPL2.VM.Memory.RTS     (RTS)
import PPL2.VM.Memory.Segment (Segment)
import PPL2.VM.Memory.Stack   (Stack)
import PPL2.VM.Memory.CodeSeg (CodeSegment)

import qualified PPL2.VM.Memory.RTS     as RTS
import qualified PPL2.VM.Memory.Segment as Segment
import qualified PPL2.VM.Memory.Stack   as Stack
import qualified PPL2.VM.Memory.CodeSeg as CodeSeg

-- ----------------------------------------

data MState v = MS
                { instr  :: !CodeSegment
                , pc     :: !CodeRef
                , stack  :: !(Stack v)
                , mem    :: !(Segment v)
                , frames :: !(RTS v)
                , status :: !(MStatus v)
                }

-- ----------------------------------------

newMState :: MState v
newMState = MS
  { instr  = CodeSeg.new []
  , pc     = nullPC
  , stack  = Stack.new
  , mem    = Segment.newInit []
  , frames = RTS.new
  , status = Ok
  }

-- ----------------------------------------

msInstr :: Lens' (MState v) CodeSegment
msInstr k ms = (\ new -> ms {instr = new}) <$> k (instr ms)

msPC :: Lens' (MState v) CodeRef
msPC k ms = (\ new -> ms {pc = new}) <$> k (pc ms)

msMem :: Lens' (MState v) (Segment v)
msMem k ms = (\ new -> ms {mem = new}) <$> k (mem ms)

msStack :: Lens' (MState v) (Stack v)
msStack k ms = (\ new -> ms {stack = new}) <$> k (stack ms)

msFrames :: Lens' (MState v) (RTS v)
msFrames k ms = (\ new -> ms {frames = new}) <$> k (frames ms)

msStatus :: Lens' (MState v) (MStatus v)
msStatus k ms = (\ new -> ms {status = new}) <$> k (status ms)

-- ----------------------------------------

data MStatus v = Ok
               | AddressViolation v
               | IllegalArgument v
               | EvalStackUnderflow
               | RTStackUnderflow
               | IllegalOpCode
               | IllegalResult
               | IOError String
               | Terminated
               deriving (Show)

statusOk :: MStatus v -> Bool
statusOk Ok = True
statusOk _  = False

statusTerminated :: MStatus v -> Bool
statusTerminated Terminated = True
statusTerminated _  = False

-- ----------------------------------------
