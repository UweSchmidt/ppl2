{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.GenCode where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.CodeGen.Types
import PPL2.CodeGen.Builder
import PPL2.CodeGen.Monad

-- ----------------------------------------

class LoadLit v where
  gLoadLit :: v -> GenCode v Code

-- ----------------------------------------

genCode :: LoadLit v => Mnemonics -> Expr v -> GenCode v Code
genCode mns = go
  where
    go e
      -- load a variable
      | Just a <- e ^? address =
          return $ gLoad a

      -- load a literal
      | Just v <- e ^? lit =
          gLoadLit v

      -- gen code for a compute instr
      | Just (opr, es) <- e ^? hasOp (`elem` mns) . expr = do
          cs <- traverse go es
          return $ gComp opr <> mconcat cs

      -- code for if-then-else
      | Just (cond, then', else') <- e ^? hasOp (== "if") . expr3 . _2 = do
          l1 <- newLabel
          l2 <- newLabel
          ccond <- go cond
          cthen <- go then'
          celse <- go else'

          return $ mconcat
            [ ccond
            , gBrFalse l1
            , cthen
            , gJump    l2
            , gLabel   l1
            , celse
            , gLabel   l2
            ]
      -- code for while loops
      | Just (cond, body) <- e ^? hasOp (== "while") . expr2 . _2 = do
          l1 <- newLabel
          l2 <- newLabel
          ccond <- go cond
          cbody <- go body

          return $ mconcat
            [ gJump   l1
            , gLabel  l2
            , cbody
            , gLabel  l1
            , ccond
            , gBrTrue l2
            ]

      | otherwise =
          abortGC $ errGCExpr e

-- ----------------------------------------
