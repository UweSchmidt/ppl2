module PPL2.Prim.Prelude
       ( module PPL2.Prim.Prelude
       , module Data.Word
       )
where

import Data.Word (Word)

-- ----------------------------------------

-- segment identifier
type SegId  = Int

-- index within a segment
type Offset = Word

-- fixed segment identifiers

dataSid, fstRTSSid, nullSid :: SegId
dataSid   = 0            -- static data segment
fstRTSSid = dataSid + 1  -- 1. rts segment
nullSid   = dataSid - 1  -- illegal segment (for null reference)

data    DataRef = DR !SegId !Offset
newtype CodeRef = CR Offset

nullRef :: DataRef
nullRef = DR nullSid 0

-- ----------------------------------------
