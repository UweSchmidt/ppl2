module PPL2.Pretty.MProg where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.Pretty.Instr
import PPL2.System.Types (MonadCompile)

-- ----------------------------------------

prettyMProg :: (Show v) =>
               (OpCode -> Maybe Mnemonic) -> (MCode, [v]) -> String
prettyMProg toMn (acode, adata) =
  unlines $
  [ "code segment"
  , "============"
  , ""
  ] ++
  prettyMCode toMn acode ++
  [ ""
  , "data segment"
  , "============"
  , ""
  , show adata  -- preliminary
  ]

-- ----------------------------------------

prettyMCode :: (OpCode -> Maybe Mnemonic) -> MCode -> [String]
prettyMCode mns is =
  zipWith pretty' [0..] is
  where
    pretty' pc' =
      prettyInstr (indent pc') (prettyOp mns) (prettyJmp pc') prettyLab

-- ----------------------------------------
