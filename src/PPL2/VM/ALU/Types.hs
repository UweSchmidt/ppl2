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

-- for program execution
toALU :: CInstrSet v -> ALU v
toALU =
  ALU . I.fromList . zip [0..] . map snd . M.toAscList . unCIS

-- for encoding assembler instructions
toOpCode :: MapMnemonics a -> Mnemonic -> Maybe OpCode
toOpCode iset mn = M.lookup mn im
  where
    ms = map fst . M.toAscList . unCIS $ iset
    im = M.fromList (zip ms [0..])

-- for tracing execution
toMnemonics :: MapMnemonics a -> Mnemonics
toMnemonics =
  map fst . M.toAscList . unCIS

-- the opcode decoding
getMicroInstr :: OpCode -> ALU v -> Maybe (MicroInstr v)
getMicroInstr oc (ALU a) = I.lookup oc a

-- ----------------------------------------
