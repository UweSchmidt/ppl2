module PPL2.VM.ALU.Types where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.VM.Control.Types (MicroInstr)

import           Data.IntMap (IntMap)
import qualified Data.IntMap as I
import           Data.Map    (Map)
import qualified Data.Map    as M
import qualified Data.List   as L

-- ----------------------------------------

type    CInstr     v = (Mnemonic, MicroInstr v)
type    CInstrList v = [CInstr v]
type    CInstrSet  v = MapMnemonics (MicroInstr v)

newtype MapMnemonics a =
  CIS {unCIS :: Map Mnemonic a}

newtype ALU v =
  ALU {unALU :: IntMap (MicroInstr v)}

-- ----------------------------------------

newCInstrSet :: [(Mnemonic, a)] -> MapMnemonics a
newCInstrSet =
  L.foldl' unionCInstrSet (CIS M.empty)
  .
  map (\ (mn, mi) -> CIS $ M.singleton mn mi)

unionCInstrSet :: MapMnemonics a -> MapMnemonics a -> MapMnemonics a
unionCInstrSet (CIS s1) (CIS s2) =
  CIS $ M.unionWithKey doubleMnemonics s1 s2

doubleMnemonics :: Mnemonic -> a -> a -> a
doubleMnemonics dmn _m1 _m2 =
  error $
  "PPL2.VI.ALU.Types': doublicated mnemonics in instruction set: "
  ++ show dmn

-- derive the ALU from the instruction set
toALU :: CInstrSet v -> ALU v
toALU =
  ALU . I.fromList . zip [0..] . map snd . M.toAscList . unCIS

-- decoding opcodes back to mnemonics
toMnemonic :: CInstrSet v -> (OpCode -> Maybe Mnemonic)
toMnemonic inset
  = flip I.lookup ocm
  where
    ocm = I.fromList . zip [0..] . map fst . M.toAscList . unCIS $ inset

-- test whether a name is a mnemonic
hasOpCode :: MapMnemonics a -> (Mnemonic -> Bool)
hasOpCode inset = isJust . toOpCode inset

-- for encoding assembler mnemonics into opcodes
toOpCode :: MapMnemonics a -> (Mnemonic -> Maybe OpCode)
toOpCode inset mn = M.lookup mn im
  where
    ms = map fst . M.toAscList . unCIS $ inset
    im = M.fromList (zip ms [0..])

-- the opcode decoding
getMicroInstr :: OpCode -> ALU v -> Maybe (MicroInstr v)
getMicroInstr oc (ALU a) = I.lookup oc a

-- ----------------------------------------
