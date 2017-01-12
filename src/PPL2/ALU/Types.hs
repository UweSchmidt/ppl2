module PPL2.ALU.Types where

import PPL2.Prim.Prelude ()
import PPL2.Prim.Instr
import PPL2.Control.Types

import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.List   as L

-- ----------------------------------------

type CompInstr    v = (Mnemonic, MicroInstr v)
type CompInstrSet v = [CompInstr v]
type Mnemonics      = [Mnemonic]

newtype ALU v       = ALU (IntMap (Mnemonic, MicroInstr v))

-- ----------------------------------------

new :: ALU v
new = ALU M.empty

addInstr :: CompInstrSet v -> ALU v -> ALU v
addInstr is (ALU a) =
  ALU $ L.foldl' (\ a' (i, mi) -> M.insert i mi a') a $ zip [mx..] is
  where
    mx = maybe 0 (+ 1) $ (fst . fst) <$> M.maxViewWithKey a

get :: OpCode -> ALU v -> Maybe (CompInstr v)
get oc (ALU a) = M.lookup oc a

getMnemonic :: OpCode -> ALU v -> Maybe Mnemonic
getMnemonic oc alu = fst <$> get oc alu

getMicroInstr :: OpCode -> ALU v -> Maybe (MicroInstr v)
getMicroInstr oc alu = snd <$> get oc alu

getMnemonics :: ALU v -> Mnemonics
getMnemonics (ALU a) = fst <$> M.elems a

-- ----------------------------------------
