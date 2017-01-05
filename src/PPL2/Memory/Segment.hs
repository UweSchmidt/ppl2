module PPL2.Memory.Segment
       (Segment, MVSegment, get, put, new, toDataRef)
where

import           PPL2.Prim.Prelude
import           PPL2.Prim.MValue

import qualified Data.IntMap as M
import qualified Data.List   as L

-- ----------------------------------------

-- A segment has a size and an IntMap for storing values

data Segment a = Segment !Offset !(M.IntMap a)

type MVSegment v = Segment (MValue v)

-- ----------------------------------------

-- read from a cell of a segment

get :: Offset -> Segment a -> Maybe a
get i (Segment ub m)
  | 0 <= i && i <= ub = M.lookup (fromEnum i) m
  | otherwise         = Nothing

-- write a value into a cell of a segment

put :: Offset -> a -> Segment a -> Maybe (Segment a)
put i v (Segment ub m)
  | i <= ub    = Just (Segment ub $ M.insert (fromEnum i) v m)
  | otherwise  = Nothing

-- make a new segment and init all cell with a default value
--
-- a segment is never empty
-- it contains at least a single cell

new :: Offset -> a -> Segment a
new ub v = Segment ub seg
  where
    seg = L.foldl' (\ m i -> M.insert (fromEnum i) v m) M.empty [0..ub]

toDataRef :: Offset -> Segment a -> Maybe DataRef
toDataRef i seg = (const $ DR dataSid i) <$> get i seg

-- ----------------------------------------
