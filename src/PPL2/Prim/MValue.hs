module PPL2.Prim.MValue
where

import PPL2.Prim.Prelude
import PPL2.Prim.Values

-- ----------------------------------------

data MValue
  = VUndef          -- a marker for uninitialized cells
  | VWord Word      -- an unsigned Int as machine word (???)
  | VDRef DataRef   -- address of a cell in a data segment
  | VCRef CodeRef   -- address of an instruction

-- ----------------------------------------
--
-- prisms for accessing/constructing MValues

instance WordValue MValue where
  _Word = prism VWord
    (\ x -> case x of
        VWord w -> Right w
        _       -> Left  x
    )

instance DataRefValue MValue where
  _DataRef = prism
    (VDRef . uncurry DR)
    (\ x -> case x of
        VDRef (DR sid i) -> Right (sid, i)
        _                -> Left  x
    )

instance CodeRefValue MValue where
  _CodeRef = prism
    (VCRef . CR)
    (\ x -> case x of
        VCRef (CR i) -> Right i
        _            -> Left  x
    )

instance DefaultValue MValue where
  _Default = prism
    (const VUndef)
    (\ x -> case x of
        VUndef -> Right ()
        _      -> Left x
    )

-- ----------------------------------------
