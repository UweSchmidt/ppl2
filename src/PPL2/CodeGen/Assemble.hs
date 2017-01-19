module PPL2.CodeGen.Assemble where

import PPL2.Prelude
import PPL2.VM

-- ----------------------------------------

type ACode op   = [Instr op Label]
type MCode op   = [Instr op Displ]

type LabTab     = [(Label, Offset)]

resolveLabels :: ACode op -> MCode op
resolveLabels is =
  zipWith (compDispl labTab) [0..] is'
  where
    labTab    = buildLabTab [] 0 is
    is'       = filter noLabelInstr is

buildLabTab     :: LabTab -> Offset -> ACode op -> LabTab

buildLabTab lt _ []
    = lt

buildLabTab lt i (Label l : is)
    = buildLabTab ((l, i) : lt) i is

buildLabTab lt i (_ : is)
    = buildLabTab lt (i + 1) is

-- lookup label table

labVal          :: Label -> LabTab -> Displ
labVal lab lt
    = fromIntegral . fromMaybe 0 . lookup lab $ lt


noLabelInstr           :: Instr op lab -> Bool
noLabelInstr (Label _) = False
noLabelInstr _         = True

compDispl      :: LabTab -> Displ -> Instr op Label -> Instr op Displ
compDispl lt i = fmap lval
  where
    lval l = labVal l lt - i

-- ----------------------------------------
