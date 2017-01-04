module PPL2.Memory.CodeSeg
       (CodeSeg, get, new)
where

import PPL2.Prelude

import qualified Data.Array.IArray as A

-- ----------------------------------------

newtype CodeSeg a = CS (A.Array Word a)

-- ----------------------------------------

get :: Word -> CodeSeg a ->  Maybe a
get i (CS m)
  | 0 <= lb && i <= ub = Just $ m A.! i
  | otherwise          = Nothing
  where
    (lb, ub) = A.bounds m

new :: [a] -> CodeSeg a
new xs = CS $ A.listArray (0, toEnum $ size - 1) xs
  where
    size = length xs

-- ----------------------------------------
