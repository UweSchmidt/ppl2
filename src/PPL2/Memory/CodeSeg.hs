module PPL2.Memory.CodeSeg
       (CodeSeg, CodeSegment, get, new)
where

import PPL2.Prim.Prelude
import PPL2.Prim.Instr   (MInstr)

import qualified Data.Vector as V

-- ----------------------------------------

newtype CodeSeg a = CS (V.Vector a)

type CodeSegment  = CodeSeg MInstr

-- ----------------------------------------

get :: CodeRef -> CodeSeg a ->  Maybe a
get i (CS m) = m V.!? fromIntegral i

new :: [a] -> CodeSeg a
new = CS . V.fromList

-- ----------------------------------------
