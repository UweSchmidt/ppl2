module PPL2.CodeGen.Main where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.Types   (Expr)
import PPL2.CodeGen.Monad   (GCError(..))
import PPL2.CodeGen.GenCode (genACode)
import PPL2.Pretty.CodeGen  (prettyGCExpr)
import PPL2.Compile.Types

-- ----------------------------------------
--
-- the main entry point into codegen

genCode :: (MonadCompile m, Show v, CoreValue v) =>
           Mnemonics -> Expr v -> m (ACode, [v])
genCode mns e =
  either (issueError 2) return $
  first msg $
  genACode mns e
  where
    msg (GCE s me) =
      unlines $ [ "error in codegeneration"
                , s
                ] ++ maybe [] ((:[]) . prettyGCExpr show) me

-- ----------------------------------------
