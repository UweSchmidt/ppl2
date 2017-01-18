module PPL2.Memory.Segment
       (Segment, get, put, new, newInit, toDataRef, dump)
where

import PPL2.Prelude
import PPL2.VM.Types

import qualified Data.IntMap as M
import qualified Data.List   as L

-- ----------------------------------------

-- A segment has a size and an IntMap for storing values

data Segment a = Segment !Offset !(M.IntMap a)

-- ----------------------------------------

-- read from a cell of a segment

get :: Offset -> Segment a -> Maybe a
get i (Segment len m)
  | 0 <= i && i < len = M.lookup (fromEnum i) m
  | otherwise         = Nothing

-- write a value into a cell of a segment

put :: Offset -> a -> Segment a -> Maybe (Segment a)
put i v (Segment len m)
  | i < len    = Just (Segment len $ M.insert (fromEnum i) v m)
  | otherwise  = Nothing

-- make a new segment and init all cell with a default value
--
-- a segment is never empty
-- it contains at least a single cell

new :: Offset -> a -> Segment a
new len v = Segment len seg
  where
    seg
      | len /= 0  = L.foldl' (\ m i -> M.insert (fromEnum i) v m) M.empty [0..len - 1]
      | otherwise = M.empty

newInit :: [a] -> Segment a
newInit vs = Segment (toEnum $ M.size seg) seg
  where
    seg = L.foldl' (\ m (i, v) -> M.insert i v m) M.empty $
          zip [0..] vs

toDataRef :: Offset -> Segment a -> DataRef
toDataRef i _seg = (dataSid, i)

dump :: Segment a -> [a]
dump (Segment _len seg) = M.elems seg

-- ----------------------------------------
