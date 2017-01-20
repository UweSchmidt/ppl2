{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.Types where

import PPL2.Prelude
import PPL2.VM.Types

import Data.Tree

-- ----------------------------------------

-- | an expression is represented as a rose tree
--
-- The attributes at the inner nodes are Mnemonics of the operation
-- to be performed. At the leaves there are addresses for variable
-- access or litterals.
--
-- What a concrete literal is remains open and is determined by the type
-- parameter. How a literal is compiled remains also open, v must be an
-- instance of the LoadLit class

type Expr  v = Tree (ExOp v)
type Exprs v = [Expr v]

data ExOp v = Opr Mnemonic
            | Lit v
            | Adr Address
              deriving Show -- just for testing

-- ----------------------------------------
--
-- smart selectors/constructors for Expr-essions

address :: Prism' (Expr a) Address
address = prism
  (\ i -> Node (Adr i) [])
  (\ case
      Node (Adr i) _xs -> Right i
      x                -> Left  x
  )

lit :: Prism' (Expr a) a
lit = prism
  (\ i -> Node (Lit i) [])
  (\ case
      Node (Lit i) _xs -> Right i
      x                -> Left  x
  )

expr :: Prism' (Expr a) (Mnemonic, Exprs a)
expr = prism
  (\ (i, xs) -> Node (Opr i) xs)
  (\ case
      Node (Opr i) xs -> Right (i, xs)
      x               -> Left  x
  )

expr0 :: Prism' (Expr a) Mnemonic
expr0 = prism
  (\ i -> Node (Opr i) [])
  (\ case
      Node (Opr i) [] -> Right i
      x               -> Left  x
  )

expr1 :: Prism' (Expr a) (Mnemonic, Expr a)
expr1 = prism
  (\ (i, x) -> Node (Opr i) [x])
  (\ case
      Node (Opr i) [x] -> Right (i, x)
      x                -> Left  x
  )

expr2 :: Prism' (Expr a) (Mnemonic, (Expr a, Expr a))
expr2 = prism
  (\ (i, (x1, x2)) -> Node (Opr i) [x1, x2])
  (\ case
      Node (Opr i) [x1, x2] -> Right (i, (x1, x2))
      x                     -> Left  x
  )

expr3 :: Prism' (Expr a) (Mnemonic, (Expr a, Expr a, Expr a))
expr3 = prism
  (\ (i, (x1, x2, x3)) -> Node (Opr i) [x1, x2, x3])
  (\ case
      Node (Opr i) [x1, x2, x3] -> Right (i, (x1, x2, x3))
      x                         -> Left  x
  )

hasOp :: (Mnemonic -> Bool) -> Prism' (Expr a) (Expr a)
hasOp p = filtered p'
  where
    p' (Node (Opr i) _) = p i
    p' _                = False

-- ----------------------------------------
