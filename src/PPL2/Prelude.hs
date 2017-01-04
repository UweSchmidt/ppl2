module PPL2.Prelude
       ( module PPL2.Prelude
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
dataSid, fstRTSSid :: SegId
dataSid   = 0
fstRTSSid = dataSid + 1

data    DataRef = DR !SegId !Offset
newtype CodeRef = CR Offset

-- ----------------------------------------
