module PPL2.CodeGen.Assemble
  (assemble)
where

import PPL2.Prelude
import PPL2.VM

import qualified Data.Map as M

-- ----------------------------------------

type LabTab     = M.Map Label Offset

-- ----------------------------------------

assemble :: CInstrSet v -> ACode -> MCode
assemble inset is =
  zipWith toMInstr [0..] is'
  where
    toMInstr i = bimap opc' lval
      where
        lval l = labVal l labTab - i

    labTab     = buildLabTab M.empty 0 is
    labVal lab = fromIntegral .
                 fromMaybe
                 (error $ "PPL2.CodeGen.Assemble: undeclared label: " ++ show lab) .
                 M.lookup lab
    is'        = filter noLabelInstr is

    opc        = toOpCode inset
    opc' mn    = fromMaybe
                 (error $ "PPL2.CodeGen.Assemble: unknown mnemonic: " ++ show mn)
                 (opc mn)
    -- the errors indicate uncomplete or wrong code generation algorithms
    -- therfore the hard abort

buildLabTab     :: LabTab -> Offset -> [Instr op Label] -> LabTab
buildLabTab lt _ []             = lt
buildLabTab lt i (Label l : is) = buildLabTab (M.insert l i lt)  i      is
buildLabTab lt i (_       : is) = buildLabTab               lt  (i + 1) is

noLabelInstr           :: Instr op lab -> Bool
noLabelInstr (Label _) = False
noLabelInstr _         = True

-- ----------------------------------------
