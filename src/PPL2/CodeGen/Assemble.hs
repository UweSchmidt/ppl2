module PPL2.CodeGen.Assemble
  (assemble)
where

import PPL2.Prelude
import PPL2.VM

import qualified Data.Map as M

-- ----------------------------------------

type LabTab     = M.Map Label Offset

-- ----------------------------------------

assemble :: CInstrSet v -> ACode -> ([String], MCode)
assemble inset is =
  partitionEithers $ zipWith toMInstr [0..] is'
  where
    toMInstr :: Int -> AInstr -> Either String MInstr
    toMInstr i = go
      where
        go (Br b    l)  = Br b    <$> lval l
        go (Jump    l)  = Jump    <$> lval l
        go (SRJump  l)  = SRJump  <$> lval l
        go (LoadLab l)  = LoadLab <$> lval l
        go (Label   l)  = Label   <$> lval l
        go (Comp o)     = Comp    <$> opc  o

        go (Load  a)    = Right $ Load  a
        go (Store a)    = Right $ Store a
        go (LoadInd)    = Right   LoadInd
        go (StoreInd)   = Right   StoreInd
        go (LoadI x)    = Right $ LoadI x
        go (Pop)        = Right   Pop
        go (Dup   x)    = Right $ Dup   x
        go (Swap)       = Right   Swap
        go (LoadAddr a) = Right $ LoadAddr a
        go (JumpInd)    = Right   JumpInd
        go (SRJumpInd)  = Right   SRJumpInd
        go (Enter o)    = Right $ Enter o
        go (Leave)      = Right   Leave
        go (Term)       = Right   Term

        lval l     = (\ x -> x - i) <$> labVal l labTab

    labTab     = buildLabTab M.empty 0 is

    labVal :: Label -> LabTab -> Either String Displ
    labVal lab lt =
      fmap fromIntegral <$>
      maybe (Left $ "undeclared label: " ++ show lab) Right $
      M.lookup lab lt

    is'        = filter noLabelInstr is

    opc        :: Mnemonic -> Either String OpCode
    opc mn     = maybe
                 (Left $ "unknown mnemonic: " ++ show mn)
                 Right
                 (opc' mn)
    opc'       = toOpCode inset

    -- the errors indicate uncomplete or wrong code generation algorithms
    -- therfore the hard abort
    -- TODO: remove this hack

buildLabTab     :: LabTab -> Offset -> [Instr op Label] -> LabTab
buildLabTab lt _ []             = lt
buildLabTab lt i (Label l : is) = buildLabTab (M.insert l i lt)  i      is
buildLabTab lt i (_       : is) = buildLabTab               lt  (i + 1) is

noLabelInstr           :: Instr op lab -> Bool
noLabelInstr (Label _) = False
noLabelInstr _         = True

-- ----------------------------------------
