module PPL2.Prim.MValue
where

import PPL2.Prim.Prelude

-- ----------------------------------------

data MValue v
  = VUndef          -- a marker for uninitialized cells
  | VDRef DataRef   -- address of a cell in a data segment
  | VCRef CodeRef   -- address of an instruction
  | VWord Word      -- an unsigned Int as machine word (???)
  | VData v         -- data used in a specific machine
                    -- the "hole" in the data structure

class IsNull a where
  isNull :: a -> Bool

instance IsNull v => IsNull (MValue v) where
  isNull (VDRef (DR sid _offs)) = sid == nullSid
  isNull (VWord w)              = w == 0
  isNull (VData v)              = isNull v
  isNull _                      = False

-- ----------------------------------------
