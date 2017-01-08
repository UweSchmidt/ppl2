module PPL2.Prim.MValue
where

import PPL2.Prim.Prelude

-- ----------------------------------------

data MValue v
  = VUndef          -- a marker for uninitialized cells
  | VWord Word      -- an unsigned Int as machine word (???)
  | VDRef DataRef   -- address of a cell in a data segment
  | VCRef CodeRef   -- address of an instruction
  | VUser v         -- data used in a specific machine
                    -- the "hole" in the data structure

-- ----------------------------------------
--
-- prisms for accessing/constructing MValues

_Defined :: Prism' (MValue v) (MValue v)
_Defined = prism id
  (\ x -> case x of
      VUndef -> Left  x
      _      -> Right x
  )

vUndef :: MValue v
vUndef = VUndef

isUndef :: MValue v -> Bool
isUndef = isNothing . (^? _Defined)

-- ----------------------------------------

_Word    :: Prism' (MValue v) Word
_Word = prism VWord
  (\ x -> case x of
      VWord w -> Right w
      _       -> Left  x
  )

_Int     :: Prism' (MValue v) Int
_Int = _Word . isoWordInt
  where
    isoWordInt = iso fromEnum toEnum

_Bool    :: Prism' (MValue v) Bool
_Bool = _Word . isoWordBool
    where
      isoWordBool = iso (/= 0) (toEnum . fromEnum)

-- ----------------------------------------

_DataRef :: Prism' (MValue v) (SegId, Offset)
_DataRef = prism
  (VDRef . uncurry DR)
  (\ x -> case x of
      VDRef (DR sid i) -> Right (sid, i)
      _                -> Left  x
  )

_CodeRef :: Prism' (MValue v) Offset
_CodeRef = prism
  (VCRef . CR)
  (\ x -> case x of
      VCRef (CR i) -> Right i
      _            -> Left  x
  )

_User    :: Prism' (MValue v) v
_User = prism VUser
  (\ x -> case x of
      VUser v -> Right v
      _       -> Left  x
  )

-- ----------------------------------------
