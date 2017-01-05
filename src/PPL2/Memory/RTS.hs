module PPL2.Memory.RTS
       (RTS, MVRTS, new, push, pop, get, put, getLocal, putLocal, toDataRef)
where

import           PPL2.Prim.Prelude
import           PPL2.Prim.MValue

import           PPL2.Memory.Segment (Segment)
import qualified PPL2.Memory.Segment as Segment

import qualified Data.IntMap as M

-- ----------------------------------------

data RTS a =
  RTS
  { nextId   :: ! SegId
  , topId    :: ! SegId
  , segments :: ! (M.IntMap (SegId, Segment a))
  }

type MVRTS v = RTS (MValue v)

-- ----------------------------------------

new :: RTS a
new =
  RTS
  { nextId   = fstRTSSid
  , topId    = dataSid
  , segments = M.empty
  }

push :: Segment a -> RTS a -> RTS a
push seg (RTS nid tid segs) =
  RTS
  { nextId   = nid + 1
  , topId    = nid
  , segments = M.insert nid (tid, seg) segs
  }

pop :: RTS a -> Maybe (RTS a)
pop (RTS nid tid segs)
  | tid /= dataSid =
      do lastId <- fst <$> M.lookup tid segs
         return $ RTS nid lastId (M.delete tid segs)

  | otherwise = -- RTS underflow
      Nothing

get :: SegId -> Offset -> RTS a -> Maybe a
get sid i RTS{segments = segs} = do
  seg <- snd <$> M.lookup sid segs
  Segment.get i seg

put :: SegId -> Offset -> a -> RTS a -> Maybe (RTS a)
put sid i v rts@RTS{segments = segs} = do
  (lastId, seg) <- M.lookup sid segs
  seg'          <- Segment.put i v seg
  return $ rts {segments = M.insert sid (lastId, seg') segs}

getLocal :: Offset -> RTS a -> Maybe a
getLocal i rts = get (topId rts) i rts

putLocal :: Offset -> a -> RTS a -> Maybe (RTS a)
putLocal i v rts = put (topId rts) i v rts

toDataRef :: Offset -> RTS a -> Maybe DataRef
toDataRef i rts = (const $ DR (topId rts) i) <$> getLocal i rts

-- ----------------------------------------
