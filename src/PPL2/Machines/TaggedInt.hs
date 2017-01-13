-- this is a VM with tagged values for
-- integer arithmentic with Int's (as Word's),
-- data refs, code refs and an illegal value
-- for uninitialized values
--
-- type checking is done in the prisms,
-- this prevents implicit conversions between words and addresses
-- and operations with uninitialized values are detected

-- ----------------------------------------

module PPL2.Machines.TaggedInt
where

import PPL2.Machines.Prelude

import PPL2.ALU.IntegerArithmUnit

-- ----------------------------------------

data MV
  = VUndef                -- a marker for uninitialized cells
  | VWord !Word           -- an unsigned Int as machine word (???)
  | VDRef !SegId !Offset  -- address of a cell in a data segment
  | VCRef !CodeRef        -- address of an instruction

-- ----------------------------------------
--
-- prisms for accessing/constructing MVs

instance WordValue MV where
  _Word = prism VWord
    (\ x -> case x of
        VWord w -> Right w
        _       -> Left  x
    )

instance DataRefValue MV where
  _DataRef = prism
    (uncurry VDRef)
    (\ x -> case x of
        VDRef sid i -> Right (sid, i)
        _           -> Left  x
    )

instance CodeRefValue MV where
  _CodeRef = prism
    VCRef
    (\ x -> case x of
        VCRef i -> Right i
        _       -> Left  x
    )

instance DefaultValue MV where
  _Default = prism
    (const VUndef)
    (\ x -> case x of
        VUndef -> Right ()
        _      -> Left x
    )

-- ----------------------------------------

execProg :: Bool -> [MInstr] -> [MV] -> IO (MState MV)
execProg = execProg' alu
  where
    alu = integerArithmeticUnit `addInstr` newAlu

-- ----------------------------------------
