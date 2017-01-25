module PPL2.Pretty.UntypedExpr where

import PPL2.Prelude
import PPL2.CodeGen.Types
import PPL2.Pretty.Instr  (prettyAddr)

import Data.Tree          (drawTree)

-- ----------------------------------------

prettyUntypedExpr :: (v -> String) -> UntypedExpr v -> String
prettyUntypedExpr v2s =
  drawTree . fmap e2s
  where
    e2s (Opr m) = m
    e2s (Lit v) = v2s v
    e2s (Adr a) = prettyAddr a
    e2s (Lab l) = l ++ ":"

-- ----------------------------------------
