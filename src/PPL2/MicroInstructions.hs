{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module PPL2.MicroInstructions where

import           PPL2.Prim.Prelude
import           PPL2.Prim.Values
import           PPL2.Prim.Instr

import qualified PPL2.Memory.RTS     as RTS
import qualified PPL2.Memory.Segment as Segment
import qualified PPL2.Memory.Stack   as Stack
import qualified PPL2.Memory.CodeSeg as CodeSeg
import           PPL2.Memory.State

import           PPL2.Control.Instructions
import           PPL2.Control.MicroOperations
import           PPL2.Control.Monad
import           PPL2.ALU.Types
import           PPL2.ALU.MicroOperations

import           PPL2.Pretty.Instr

import Control.Monad.Except

import System.IO

-- ----------------------------------------

instrTrc :: ALU v -> MInstr -> Offset -> MicroInstr v
instrTrc alu ins pc' =
  io $ hPutStrLn stderr line
  where
    line = prettyInstr indent prettyOp prettyJmp prettyLab ins

    indent xs =
      fillLeft 6 (show pc') ++ ": " ++ xs

    prettyOp op' =
      fromMaybe ("not-used-" ++ show op') $ getMnemonic op' alu

    prettyJmp disp =
      [show disp, "--> " ++ show (pc' + toEnum disp)]

    prettyLab disp =
      show disp ++ ":"

-- ----------------------------------------

runCPU :: (CodeRefValue v, DataRefValue v, DefaultValue v, WordValue v) =>
          Bool -> ALU v -> MicroInstr v
runCPU trc alu = go
  where
    go = do
      continue <- statusOk <$> use msStatus
      when continue $ do
        pc    <- getPC
        instr <- getInstr pc
        incrPC

        -- trace the instructions
        when trc $ instrTrc alu instr pc

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

initMemory :: [MInstr] -> [v] -> MicroInstr v
initMemory is vs = do
  msInstr .= CodeSeg.new is
  setPC 0
  msMem    .= Segment.newInit vs
  msStack  .= Stack.new
  msFrames .= RTS.new
  msStatus .= Ok

-- ----------------------------------------
