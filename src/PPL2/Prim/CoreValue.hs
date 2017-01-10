module PPL2.Prim.CoreValue
where

import PPL2.Prim.Prelude
import PPL2.Prim.Values

-- ----------------------------------------

data CoreValue
  = VUndef                -- a marker for uninitialized cells
  | VWord !Word           -- an unsigned Int as machine word (???)
  | VDRef !SegId !Offset  -- address of a cell in a data segment
  | VCRef CodeRef         -- address of an instruction

-- ----------------------------------------
--
-- prisms for accessing/constructing CoreValues

instance WordValue CoreValue where
  _Word = prism VWord
    (\ x -> case x of
        VWord w -> Right w
        _       -> Left  x
    )

instance DataRefValue CoreValue where
  _DataRef = prism
    (uncurry VDRef)
    (\ x -> case x of
        VDRef sid i -> Right (sid, i)
        _           -> Left  x
    )

instance CodeRefValue CoreValue where
  _CodeRef = prism
    VCRef
    (\ x -> case x of
        VCRef i -> Right i
        _       -> Left  x
    )

instance DefaultValue CoreValue where
  _Default = prism
    (const VUndef)
    (\ x -> case x of
        VUndef -> Right ()
        _      -> Left x
    )

-- ----------------------------------------
