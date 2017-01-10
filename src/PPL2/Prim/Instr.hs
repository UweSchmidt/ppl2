module PPL2.Prim.Instr
where

import           PPL2.Prim.Prelude

-- ----------------------------------------

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
  | Comp op              -- process values on eval stack
  | Term                 -- terminated program run
  | Label   lab          -- pseudo instr for assembler code gen
                         -- will be removed during assembly, acts a noop

-- machine instructions
type MInstr = Instr OpCode Displ

-- assembler instructions
type AInstr = Instr Label

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
