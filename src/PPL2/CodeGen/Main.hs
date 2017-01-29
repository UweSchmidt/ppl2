module PPL2.CodeGen.Main where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.UntypedExpr (UntypedExpr)
import PPL2.CodeGen.Monad       (GCError(..))
import PPL2.CodeGen.GenCode     (genACode)
import PPL2.Pretty.UntypedExpr  (prettyUntypedExpr)
import PPL2.System.Types

-- ----------------------------------------
--
-- the main entry point into codegen

genCode :: (MonadCompile m, Show v, CoreValue v) =>
           (Mnemonic -> Bool) -> UntypedExpr v -> m (AProg v)
genCode isop e =
  either (issueError 2017) return $
  first msg $
  genACode isop e
  where
    msg (GCE s me) =
      unlines $ [ "error in codegeneration"
                , s
                ] ++ maybe [] ((:[]) . prettyUntypedExpr show) me

-- ----------------------------------------
