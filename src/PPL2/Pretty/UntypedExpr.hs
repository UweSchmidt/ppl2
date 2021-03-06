module PPL2.Pretty.UntypedExpr where

import PPL2.Prelude
import PPL2.CodeGen.UntypedExpr
import PPL2.Pretty.Instr  (prettyAddr)

import Data.Tree          (drawTree)
import Data.Char

-- ----------------------------------------

prettyUntypedExpr :: (v -> String) -> UntypedExpr v -> String
prettyUntypedExpr v2s =
  drawTree . fmap e2s
  where
    e2s (Opr m) = op2s m
    e2s (Lit v) = v2s v
    e2s (Adr a) = prettyAddr a
    e2s (Lab l) = l ++ ":"

    op2s o@(x : xs)
      -- | isAlpha x = o
      | otherwise = "(" ++ o ++ ")"

-- ----------------------------------------
