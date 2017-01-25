module PPL2.VM.Control.Loop where

import PPL2.Prelude
import PPL2.VM.Types

import qualified PPL2.VM.Memory.CodeSeg       as CodeSeg
import qualified PPL2.VM.Memory.Segment       as Segment
import           PPL2.VM.Memory.State         (MState, newMState
                                              ,msInstr, msMem, msStatus, statusOk)
import           PPL2.VM.Control.Instructions
import           PPL2.VM.Control.MicroOps     (getInstr, getPC, incrPC)
import           PPL2.VM.Control.Types        (MicroInstr, runMicroCode, io)
import           PPL2.VM.ALU.Types
import           PPL2.Pretty.Instr            (instrTrc)

import           System.IO                    (stderr, hPutStrLn)

import           PPL2.System.Types

-- ----------------------------------------

runProg :: (MonadIO m, CoreValue v) =>
           CInstrSet v -> Bool -> [MInstr] -> [v] -> m (MState v)
runProg instrset trc mcode mdata = do
  s1 <- liftIO $ runMicroCode (initMem >> execLoop trcOutput instrset) newMState
  return $ snd s1
  where
    initMem = do
      msInstr .= CodeSeg.new     mcode
      msMem   .= Segment.newInit mdata
    trcOutput
      | trc       = io . hPutStrLn stderr
      | otherwise = const $ return ()

-- ----------------------------------------

execLoop :: CoreValue v =>
          (String -> MicroInstr v) ->    -- the trace output cmd
          CInstrSet v ->                 -- the instruction set
          MicroInstr v
execLoop trc inset = go
  where
    alu = toALU inset

    instructionTrace = instrTrc (toMnemonic inset) trc

    go = do
      continue <- statusOk <$> use msStatus
      when continue $ do
        pc    <- getPC
        instr <- getInstr pc
        incrPC

        instructionTrace instr pc

        case instr of
          Load  a    -> iLoad a
          Store a    -> iStore a
          Comp oc    -> iComp alu oc
          LoadInd    -> iLoadInd
          StoreInd   -> iStoreInd
          LoadI i    -> iLoadI i
          Pop        -> iPop
          Dup   i    -> iDup i
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

-- ----------------------------------------
