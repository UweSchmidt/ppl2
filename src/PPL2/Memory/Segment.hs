module PPL2.Memory.Segment
       (Segment', get, put, new)
where

import qualified Data.IntMap as M
import qualified Data.List   as L

-- ----------------------------------------

-- A segment has a size and an IntMap for storing values

data Segment' a = Segment !Int !(M.IntMap a)

-- ----------------------------------------

-- read from a cell of a segment

get :: Int -> Segment' a -> Maybe a
get i (Segment size m)
  | 0 <= i && i < size = M.lookup i m
  | otherwise          = Nothing

-- write a value into a cell of a segment

put :: Int -> a -> Segment' a -> Maybe (Segment' a)
put i v (Segment size m)
  | 0 <= i && i < size = Just (Segment size $ M.insert i v m)
  | otherwise          = Nothing

-- make a new segment and init all cell with a default value

new :: Int -> a -> Segment' a
new size v = Segment (size `max` 0) seg
  where
    seg = L.foldl' (\ m i -> M.insert i v m) M.empty [0.. size - 1]

-- ----------------------------------------
