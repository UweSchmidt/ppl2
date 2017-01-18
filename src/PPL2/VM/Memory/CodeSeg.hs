module PPL2.VM.Memory.CodeSeg
       (CodeSeg, CodeSegment, get, new)
where

import PPL2.Prelude
import PPL2.VM.Types

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
