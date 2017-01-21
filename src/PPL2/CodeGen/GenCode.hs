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

      -- code for tuple construction
      -- and sequences of expressions, in C ","-expressions
      | Just es <- e ^? hasOp (`elem` [",", ";"]) . expr . _2 =
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

      -- code for tuple deconstruction
      -- a sequence of stores as in  x,y := y,x
      | Just as <- e ^? hasOp (== ",") . expr . _2 =
          mconcat <$> traverse store (reverse as)

      -- an indirect store with computed address
      | Just ae <- e ^? hasOp (== "*") . expr1 . _2 = do
          cae <- go ae
          return $ cae <> gStoreInd

      -- discard a value, like in C expr statements, <expr>;
      -- move the result of an expression into a black hole
      | Just _ <- e ^? hasOp (== "pop") . expr0 =
          return $ gPop

      | otherwise =
          abortGC $ errGCExpr e

    branch lab b e
      | Just e1 <- e ^? hasOp (== "not") . expr1 . _2 =
          branch lab (not b) e1
      | Just (e1, e2) <- e ^? hasOp (== "&&") . expr2 . _2 =
          case b of
            True -> do
              lab1 <- newLabel
              ce1 <- branch lab1 (not b) e1
              ce2 <- branch lab       b  e2
              return $ ce1 <> ce2 <> gLabel lab1
            _ -> do
              ce1 <- branch lab b e1
              ce2 <- branch lab b e2
              return $ ce1 <> ce2

      | otherwise =
          abortGC $ errGCExpr e

-- ----------------------------------------
