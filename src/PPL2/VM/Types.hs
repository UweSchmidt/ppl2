{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module PPL2.VM.Types
where

import PPL2.Prelude

-- ----------------------------------------
--
-- the instruction set of all machines
--
-- op may be a Mnemonic in assembler like instructions
-- or an Int for machine instructions
--
-- lab is a symbolic position in the code
-- or a distance from current pc in a machine

data Instr op lab
  = Load  Address        -- load a value from data segment
  | Store Address        -- store a value into data segment
  | LoadInd              -- load indirect via ref from eval stack
  | StoreInd             -- store indirect via ref from eval stack
  | LoadI Int            -- load an int onto eval stack
  | Pop                  -- remove a value from top of evaluation stack
  | Dup                  -- duplicate the value on top of eval stack
  | Swap                 -- swap the 2 topmost values on eval stack
  | LoadAddr Address     -- load effective address onto eval stack
  | Br Bool lab          -- conditional jump
  | Jump    lab          -- unconditional jump
  | SRJump  lab          -- subroutine jump
  | LoadLab lab          -- load a code address onto eval stack
  | JumpInd              -- computed jump, target is on eval stack
  | SRJumpInd            -- computed subroutine jump
  | Enter Offset         -- allocate new stack frame of specific size
  | Leave                -- delete topmost stack frame
  | Comp op              -- process values on eval stack,
                         -- every op must be associated with a compute function
  | Term                 -- terminated program run
  | Label   lab          -- pseudo instr for assembler code gen
                         -- will be removed during assembly, acts a noop

instance Bifunctor Instr where
  bimap _ g (Br b    l)  = Br b    (g l)
  bimap _ g (Jump    l)  = Jump    (g l)
  bimap _ g (SRJump  l)  = SRJump  (g l)
  bimap _ g (LoadLab l)  = LoadLab (g l)
  bimap _ g (Label   l)  = Label   (g l)

  bimap f _ (Comp o)     = Comp    (f o)

  bimap _ _ (Load  a)    = Load  a
  bimap _ _ (Store a)    = Store a
  bimap _ _ (LoadInd)    = LoadInd
  bimap _ _ (StoreInd)   = StoreInd
  bimap _ _ (LoadI i)    = LoadI i
  bimap _ _ (Pop)        = Pop
  bimap _ _ (Dup)        = Dup
  bimap _ _ (Swap)       = Swap
  bimap _ _ (LoadAddr a) = LoadAddr a
  bimap _ _ (JumpInd)    = JumpInd
  bimap _ _ (SRJumpInd)  = SRJumpInd
  bimap _ _ (Enter o)    = Enter o
  bimap _ _ (Leave)      = Leave
  bimap _ _ (Term)       = Term

instance Functor (Instr op) where
  fmap = bimap id

-- ----------------------------------------

-- machine instructions
type MInstr = Instr OpCode Displ

-- assembler instructions
type AInstr = Instr Mnemonic Label

-- the opcode for the configuable set of operations
type OpCode       = Int

-- symbolic code points for assembler
type Label        = String

-- the jump distance, a signed number
type Displ        = Int

-- addresses reference a cell in the main data segment or in the top stack frame
data Address      = LocA Offset
                  | AbsA Offset

-- symbolic opcodes for assembler instructions
type Mnemonic     = String

-- ----------------------------------------
--
-- in all VMs we always need these kinds of values
-- independently of their representation
--
-- to abstract over the concrete representation within
-- a specific VM the classes defined prisms to read
-- and convert a concrete machine value into a
-- specific type (preview, ^?) and to construct a
-- concrete machine value (review, #)
--
-- a machine word may be interpeted as unsigned or signed value or as boolean
--
-- for references to program variable we need data refs,
-- for code pointers code refs
--
-- the default prism may be used for initialization and/or
-- test against uninitialized variables (_Default may construct an illegal value)
--
-- How these values are encoded in a machine value v is hidden
-- by the prisms, these act as selectors and as smart constructors

class WordValue v where
  _Word    :: Prism' v Word
  _Int     :: Prism' v Int
  _Bool    :: Prism' v Bool

  _Int = _Word . isoWordInt
    where
      isoWordInt = iso fromIntegral fromIntegral

  _Bool = _Word . isoWordBool
    where
      isoWordBool = iso (/= 0) (toEnum . fromEnum)

class DataRefValue v where
  _DataRef :: Prism' v DataRef

class CodeRefValue v where
  _CodeRef :: Prism' v Offset

class DefaultValue v where
  _Default :: Prism' v ()

type CoreValue v = (WordValue v, DataRefValue v, CodeRefValue v, DefaultValue v)

-- ----------------------------------------
--
-- application specific values
--
-- doubles and strings as example

class DoubleValue v where
  _Double :: Prism' v String

class StringValue v where
  _String :: Prism' v String

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
