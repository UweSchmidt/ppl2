module PPL2.ALU.Types where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.Control.Types    (MicroInstr)

import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.List   as L

-- ----------------------------------------

type CompInstr    v = (Mnemonic, MicroInstr v)
type CompInstrSet v = [CompInstr v]
type Mnemonics      = [Mnemonic]

newtype ALU v       = ALU (IntMap (CompInstr v))

-- ----------------------------------------

newAlu :: ALU v
newAlu = ALU M.empty

addInstr :: CompInstrSet v -> ALU v -> ALU v
addInstr is (ALU a)
  | null dmn  = alu
  | otherwise = error $
                "PPL2.ALU.Types.addInstr: doublicated mnemonics " ++ show dmn
  where
    alu = ALU $ L.foldl' (\ a' (i, mi) -> M.insert i mi a') a $ zip [mx..] is
    mx  = maybe 0 (+ 1) $ (fst . fst) <$> M.maxViewWithKey a
    dmn = dup $ getMnemonics alu

get :: OpCode -> ALU v -> Maybe (CompInstr v)
get oc (ALU a) = M.lookup oc a

getMnemonic :: OpCode -> ALU v -> Maybe Mnemonic
getMnemonic oc alu = fst <$> get oc alu

getMicroInstr :: OpCode -> ALU v -> Maybe (MicroInstr v)
getMicroInstr oc alu = snd <$> get oc alu

getMnemonics :: ALU v -> Mnemonics
getMnemonics (ALU a) = fst <$> M.elems a

-- ----------------------------------------
