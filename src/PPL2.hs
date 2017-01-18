module Main where

import PPL2.Prelude
import PPL2.VM

import qualified PPL2.Machines.UntaggedInt as U
import qualified PPL2.Machines.TaggedInt   as T

import PPL2.Memory.State
import qualified PPL2.Memory.Segment as Segment

p1 :: [MInstr]
p1 =
  [ Load (AbsA 0)
  , Load (AbsA 1)
  , Comp 4                -- addi
  , Store (AbsA 2)
  , Term
  ]

m1 :: [U.MV]
m1 =
  [ _Int # 1
  , _Int # 2
  , _Default # ()
  ]

main :: IO ()
main = do
  (MS instr pc stack mem frames status) <- U.execProg True p1 m1
  print $ Segment.dump mem
  return ()
