module Main where

import PPL2.Prelude
import PPL2.VM
import PPL2.CodeGen
import PPL2.Pretty.Instr
import PPL2.Compile.Types

import qualified PPL2.VM.Machines.UntaggedInt as U
import qualified PPL2.VM.Machines.TaggedInt   as T

import           PPL2.VM.Memory.State
import qualified PPL2.VM.Memory.Segment as Segment


genCode' :: Expr U.MV -> RunCompile (ACode, [U.MV])
genCode' = genCode (toMnemonics U.instrSet)

assemble' = undefined

execProg' = undefined

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
  -- , Swap  -- no swap, address is on top of stack, the value below
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

-- p1 :: MCode
-- p1 = assemble U.instrSet p1'

m1 :: [U.MV]
m1 = m1'

p2 :: MCode
(e2, p2) = assembleCode T.instrSet p1'

m2 :: [T.MV]
m2 = m1'

main :: IO ()
main = do
  putStrLn "assembler code"
  putStrLn $ prettyACode p1'

  let (es, p1) = assembleCode U.instrSet p1'
  unless (null es) $ do
     die $ unlines ("assembly errors:" : es)

  putStrLn "machine code"
  putStrLn $ prettyMCode (toMnemonics U.instrSet) p1

  putStrLn "exec program"
  (MS instr pc stack mem frames status) <- runProg U.instrSet True p1 m1

  putStrLn $ "\nmachine status register: " ++ show status

  putStrLn   "\nglobal memory dump"
  print $ Segment.dump mem

  return ()

main2 :: IO ()
main2 = do
  (MS instr pc stack mem frames status) <- T.execProg True p2 m2
  print $ Segment.dump mem
  print $ status
  return ()
