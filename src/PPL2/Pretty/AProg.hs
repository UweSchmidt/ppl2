module PPL2.Pretty.AProg where

import PPL2.Prelude
import PPL2.VM.Types     (ACode, AInstr)
import PPL2.Pretty.Instr (prettyInstr, fillLeft)
import PPL2.System.Types (MonadCompile)

-- ----------------------------------------

teeAProg :: (MonadCompile m, Show v) =>
            (String -> m ()) -> (ACode, [v]) -> m (ACode, [v])
teeAProg out prog = outAProg out prog >> return prog

outAProg :: (MonadCompile m, Show v) =>
            (String -> m ()) -> (ACode, [v]) -> m ()
outAProg out = out . prettyAProg

prettyAProg :: (Show v) => (ACode, [v]) -> String
prettyAProg (acode, adata) =
  unlines $
  [ "code segment"
  , "============"
  , ""
  ] ++
  prettyACode acode ++
  [ ""
  , "data segment"
  , "============"
  , ""
  , show adata  -- preliminary
  ]

prettyACode :: ACode -> [String]
prettyACode is =
  map pretty' is
  where
    pretty' :: AInstr -> String
    pretty' =
      prettyInstr indent' id prettyJmp' prettyLab'

    indent'      = (fillLeft 8 "" ++)
    prettyJmp'   = (:[])
    prettyLab' l = l ++ ":"

-- ----------------------------------------
