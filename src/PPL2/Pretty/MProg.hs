module PPL2.Pretty.MProg where

import PPL2.VM.Types
import PPL2.Pretty.Instr

-- ----------------------------------------

prettyMProg :: (Show v) =>
               (OpCode -> Maybe Mnemonic) -> (MCode, [v]) -> String
prettyMProg toMn (mcode, mdata) =
  unlines $
  [ "code segment"
  , "============"
  , ""
  ] ++
  prettyMCode toMn mcode ++
  [ ""
  , "data segment"
  , "============"
  , ""
  , prettyData mdata
  ]

-- ----------------------------------------

prettyMCode :: (OpCode -> Maybe Mnemonic) -> MCode -> [String]
prettyMCode mns is =
  zipWith pretty' [0..] is
  where
    pretty' pc' =
      prettyInstr (indent pc') (prettyOp mns) (prettyJmp pc') prettyLab

prettyData :: Show v => [v] -> String
prettyData =
  unlines . zipWith cell [0::Int ..]
  where
    cell i v = fmt' $ [show i, show v]

-- ----------------------------------------
