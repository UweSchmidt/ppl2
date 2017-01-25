module PPL2.CodeGen.Main where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.Types   (Expr)
import PPL2.CodeGen.Monad   (GCError(..))
import PPL2.CodeGen.GenCode (genACode)
import PPL2.Pretty.CodeGen  (prettyGCExpr)
import PPL2.System.Types

-- ----------------------------------------
--
-- the main entry point into codegen

genCode :: (MonadCompile m, Show v, CoreValue v) =>
           (Mnemonic -> Bool) -> Expr v -> m (ACode, [v])
genCode isop e =
  either (issueError 2) return $
  first msg $
  genACode isop e
  where
    msg (GCE s me) =
      unlines $ [ "error in codegeneration"
                , s
                ] ++ maybe [] ((:[]) . prettyGCExpr show) me

-- ----------------------------------------
