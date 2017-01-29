{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.UntypedExpr where

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

type UntypedExpr  v = Tree (UntypedExOp v)

type UntypedExprs v = [UntypedExpr v]

data UntypedExOp  v = Opr Mnemonic
                    | Lit v
                    | Adr Address
                    | Lab Label
                    deriving Show -- just for testing

-- ----------------------------------------
--
-- smart selectors/constructors for Expr-essions

address :: Prism' (UntypedExpr v) Address
address = prism
  (\ i -> Node (Adr i) [])
  (\ case
      Node (Adr i) _xs -> Right i
      x                -> Left  x
  )

label :: Prism' (UntypedExpr v) Label
label = prism
  (\ i -> Node (Lab i) [])
  (\ case
      Node (Lab i) _xs -> Right i
      x                -> Left  x
  )

lit :: Prism' (UntypedExpr v) v
lit = prism
  (\ i -> Node (Lit i) [])
  (\ case
      Node (Lit i) _xs -> Right i
      x                -> Left  x
  )

expr :: Prism' (UntypedExpr v) (Mnemonic, UntypedExprs v)
expr = prism
  (\ (i, xs) -> Node (Opr i) xs)
  (\ case
      Node (Opr i) xs -> Right (i, xs)
      x               -> Left  x
  )

expr0 :: Prism' (UntypedExpr v) Mnemonic
expr0 = prism
  (\ i -> Node (Opr i) [])
  (\ case
      Node (Opr i) [] -> Right i
      x               -> Left  x
  )

expr1 :: Prism' (UntypedExpr v) (Mnemonic, UntypedExpr v)
expr1 = prism
  (\ (i, x) -> Node (Opr i) [x])
  (\ case
      Node (Opr i) [x] -> Right (i, x)
      x                -> Left  x
  )

expr2 :: Prism' (UntypedExpr v) (Mnemonic, (UntypedExpr v, UntypedExpr v))
expr2 = prism
  (\ (i, (x1, x2)) -> Node (Opr i) [x1, x2])
  (\ case
      Node (Opr i) [x1, x2] -> Right (i, (x1, x2))
      x                     -> Left  x
  )

expr3 :: Prism' (UntypedExpr v) (Mnemonic, (UntypedExpr v, UntypedExpr v, UntypedExpr v))
expr3 = prism
  (\ (i, (x1, x2, x3)) -> Node (Opr i) [x1, x2, x3])
  (\ case
      Node (Opr i) [x1, x2, x3] -> Right (i, (x1, x2, x3))
      x                         -> Left  x
  )

hasOp :: (Mnemonic -> Bool) -> Prism' (UntypedExpr v) (UntypedExpr v)
hasOp p = filtered p'
  where
    p' (Node (Opr i) _) = p i
    p' _                = False

-- ----------------------------------------
