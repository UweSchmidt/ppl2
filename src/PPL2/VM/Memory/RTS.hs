module PPL2.VM.Memory.RTS
       (RTS, new, push, pop, get, put, getLocal, putLocal, toDataRef)
where

import PPL2.Prelude
import PPL2.VM.Types

import           PPL2.VM.Memory.Segment (Segment)
import qualified PPL2.VM.Memory.Segment as Segment

import qualified Data.IntMap as M

-- ----------------------------------------

data RTS v =
  RTS
  { nextId   :: ! SegId
  , topId    :: ! SegId
  , segments :: ! (M.IntMap (SegId, Segment v))
  }

-- ----------------------------------------

new :: RTS v
new =
  RTS
  { nextId   = fstRTSSid
  , topId    = dataSid
  , segments = M.empty
  }

push :: Segment v -> RTS v -> RTS v
push seg (RTS nid tid segs) =
  RTS
  { nextId   = nid + 1
  , topId    = nid
  , segments = M.insert nid (tid, seg) segs
  }

pop :: RTS v -> Maybe (RTS v)
pop (RTS nid tid segs)
  | tid /= dataSid =
      do lastId <- fst <$> M.lookup tid segs
         return $ RTS nid lastId (M.delete tid segs)

  | otherwise = -- RTS underflow
      Nothing

get :: SegId -> Offset -> RTS v -> Maybe v
get sid i RTS{segments = segs} = do
  seg <- snd <$> M.lookup sid segs
  Segment.get i seg

put :: SegId -> Offset -> v -> RTS v -> Maybe (RTS v)
put sid i v rts@RTS{segments = segs} = do
  (lastId, seg) <- M.lookup sid segs
  seg'          <- Segment.put i v seg
  return $ rts {segments = M.insert sid (lastId, seg') segs}

getLocal :: Offset -> RTS v -> Maybe v
getLocal i rts = get (topId rts) i rts

putLocal :: Offset -> v -> RTS v -> Maybe (RTS v)
putLocal i v rts = put (topId rts) i v rts

toDataRef :: Offset -> RTS v -> DataRef
toDataRef i rts = (topId rts, i)

-- ----------------------------------------
