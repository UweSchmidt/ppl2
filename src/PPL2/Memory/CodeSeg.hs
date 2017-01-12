module PPL2.Memory.CodeSeg
       (CodeSeg, CodeSegment, get, new)
where

import PPL2.Prim.Prelude
import PPL2.Prim.Instr   (MInstr)

import qualified Data.Array.IArray as A

-- ----------------------------------------

data CodeSeg a = CS !Word (A.Array Word a)

type CodeSegment  = CodeSeg MInstr

-- ----------------------------------------

get :: CodeRef -> CodeSeg a ->  Maybe a
get i (CS len m)
  | i < len   = Just $ m A.! i
  | otherwise = Nothing
  where
    (lb, ub) = A.bounds m

new :: [a] -> CodeSeg a
new [] = CS 0            undefined
new xs = CS (toEnum len) (A.listArray (0, toEnum $ len - 1) xs)
  where
    len = length xs

-- ----------------------------------------
