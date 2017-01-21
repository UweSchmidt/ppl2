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

      -- load indirect, dereference
      | Just ae <- e ^? hasOp (== "*") . expr1 . _2 = do
          cae <- go ae
          return $ cae <> gLoadInd

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

      -- code for a sequence of values
      | Just es <- e ^? hasOp (== ",") . expr . _2 =
          mconcat <$> traverse go es

      -- code for single and multiple assignments
      | Just (lhs, rhs) <- e ^? hasOp (== ":=") . expr2 . _2 = do
          crhs <- go rhs
          clhs <- store lhs
          return $ crhs <> clhs

      | otherwise =
          abortGC $ errGCExpr e

    -- code for storing values
    store e
      -- store under given address
      | Just a <- e ^? address =
          return $ gStore a

      -- a sequence of stores as in  x,y := y,x
      | Just as <- e ^? hasOp (== ",") . expr . _2 =
          mconcat <$> traverse store (reverse as)

      -- an indirect store with computed address
      | Just ae <- e ^? hasOp (== "*") . expr1 . _2 = do
          cae <- go ae
          return $ cae <> gStoreInd

      -- discard a value, like (void) := .... or in C <expr>;
      | Just _ <- e ^? hasOp (== "pop") . expr0 =
          return $ gPop

-- ----------------------------------------
