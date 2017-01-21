module Main where

import PPL2.Prelude
import PPL2.VM
import PPL2.CodeGen

import qualified PPL2.VM.Machines.UntaggedInt as U
import qualified PPL2.VM.Machines.TaggedInt   as T

import           PPL2.VM.Memory.State
import qualified PPL2.VM.Memory.Segment as Segment

p1' :: ACode
p1' =
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
  , LoadAddr (AbsA 1)
  , LoadInd
  , LoadAddr (AbsA 4)
--  , Swap  -- no swap, address is on top of stack, the value below
  , StoreInd
  , Term
  ]

m1' :: CoreValue v => [v]
m1' =
  [ _Int # 1
  , _Int # 2
  , _Default # ()
  , _Default # ()
  , _Default # ()
  ]

p1 :: MCode
p1 = assemble U.instrSet p1'

m1 :: [U.MV]
m1 = m1'

p2 :: MCode
p2 = assemble T.instrSet p1'

m2 :: [T.MV]
m2 = m1'

main :: IO ()
main = do
  (MS instr pc stack mem frames status) <- U.execProg True p1 m1
  print $ Segment.dump mem
  print $ status
  return ()

main2 :: IO ()
main2 = do
  (MS instr pc stack mem frames status) <- T.execProg True p2 m2
  print $ Segment.dump mem
  print $ status
  return ()
