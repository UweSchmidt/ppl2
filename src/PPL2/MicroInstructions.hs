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
import           PPL2.Control.Loop
import           PPL2.Control.MicroOps
import           PPL2.Control.Types
import           PPL2.ALU.Types
import           PPL2.ALU.MicroOps

import           PPL2.Pretty.Instr

import           Control.Monad.Except

import           System.IO

-- ----------------------------------------

initMemory :: [MInstr] -> [v] -> MicroInstr v
initMemory is vs = do
  msInstr .= CodeSeg.new is
  setPC 0
  msMem    .= Segment.newInit vs
  msStack  .= Stack.new
  msFrames .= RTS.new
  msStatus .= Ok

-- ----------------------------------------
