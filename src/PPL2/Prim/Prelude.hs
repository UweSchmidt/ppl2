{-# LANGUAGE RankNTypes #-}

module PPL2.Prim.Prelude
       ( module PPL2.Prim.Prelude
       , module Control.Lens
       , module Data.Maybe
       , module Data.Word
       )
where

import Control.Lens

import Data.Maybe hiding (fromJust)
import Data.Word (Word)

-- ----------------------------------------

-- segment identifier
type SegId  = Int

-- index within a segment
type Offset = Word

type DataRef = (SegId, Offset)

type CodeRef = Offset

-- fixed segment identifiers

dataSid, fstRTSSid, nullSid :: SegId
dataSid   = 1            -- static data segment
fstRTSSid = dataSid + 1  -- 1. rts segment
nullSid   = dataSid - 1  -- illegal segment (for null reference)

nullRef :: DataRef
nullRef = (nullSid, 0)

nullPC :: CodeRef
nullPC = 0

-- ----------------------------------------
--
-- infix pairs fto avoid parents

infixr 1 |->

(|->) :: a -> b -> (a, b)
x |-> y = (x, y)

-- a predicate as prism

predP :: (a -> Bool) -> Prism' a a
predP p = prism id
  (\ x -> case p x of
      True -> Right x
      _    -> Left  x
  )

-- ----------------------------------------
