{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module PPL2.Control.Loop where

import           PPL2.Prim.Prelude
import           PPL2.Prim.Values
import           PPL2.Prim.Instr           (Instr(..))

import           PPL2.Memory.State         (msStatus, statusOk)

import           PPL2.Control.Instructions
import           PPL2.Control.MicroOps     (getInstr, getPC, incrPC)
import           PPL2.Control.Types           (MicroInstr)

import           PPL2.ALU.Types            (ALU, getMnemonics)

import           PPL2.Pretty.Instr         (instrTrc)

-- ----------------------------------------


runCPU :: (CodeRefValue v, DataRefValue v, DefaultValue v, WordValue v) =>
          (String -> MicroInstr v) ->    -- the trace output cmd
          ALU v   ->                     -- the arithmetic locical unit
          MicroInstr v
runCPU trc alu = go
  where
    instructionTrace = instrTrc (getMnemonics alu) trc

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

-- ----------------------------------------
