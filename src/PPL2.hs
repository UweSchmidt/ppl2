module Main where

import PPL2.Prelude
import PPL2.VM
import PPL2.CodeGen.Assemble

import qualified PPL2.VM.Machines.UntaggedInt as U
import qualified PPL2.VM.Machines.TaggedInt   as T

import           PPL2.VM.Memory.State
import qualified PPL2.VM.Memory.Segment as Segment

p1 :: MCode
p1 = assemble U.instrSet
  [ Load (AbsA 0)
  , Load (AbsA 1)
  , Comp "addi"
  , Store (AbsA 2)
  , Load (AbsA 0)
  , Load (AbsA 1)
  , Comp "eqi"
  , Br True "l1"
  , LoadI 42
  , Store (AbsA 3)
  , Jump "l2"
  , Label "l1"
  , Load (AbsA 0)
  , Load (AbsA 1)
  , Comp "maxi"
  , Store (AbsA 3)
  , Label "l2"
  , Term
  ]

m1 :: [U.MV]
m1 =
  [ _Int # 1
  , _Int # 2
  , _Default # ()
  , _Default # ()
  ]

main :: IO ()
main = do
  (MS instr pc stack mem frames status) <- U.execProg True p1 m1
  print $ Segment.dump mem
  return ()
