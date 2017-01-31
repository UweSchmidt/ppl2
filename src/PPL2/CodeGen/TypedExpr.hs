{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.TypedExpr where

import PPL2.Prelude
import PPL2.VM.Types

import Data.Tree
import Data.List (intercalate)

-- ----------------------------------------

{- |

typed expressions are represented as rosetrees
with a pair as attribute, this pair conststs of
an expression op and a type

the expression op is (for inner nodes) a Mnemonic
for leaves a literal or an identifier

there are a few predefined types

-}

type Typ          = Tree Name

type TypedExpr  v = Tree (TypedExOp v, Typ)

type TypedExprs v = [TypedExpr v]

data TypedExOp  v = TOpr Mnemonic
                  | TLit v
                  | TIdent Name
                  deriving Show -- just for testing


type Name         = String

-- ----------------------------------------
--
-- smart selectors/constructors for Typ-es

tSimple :: Prism' Typ Name
tSimple = prism
  (\ i -> Node i [])
  (\ case
      Node i [] -> Right i
      x         -> Left  x
  )

tSimple' :: Name -> Prism' Typ ()
tSimple' n = prism
  (\ _ -> Node n [])
  (\ case
      Node i []
        | i == n -> Right ()
      x          -> Left  x
  )

tInt, tBool, tVoid :: Prism' Typ ()
tInt  = tSimple' "Int"
tBool = tSimple' "Bool"
tVoid = tSimple' "(,)"

tSize :: Typ -> Int
tSize t
  | Just () <- t ^? tVoid   = 0
  | Just _  <- t ^? tSimple = 1



{- }
-- ----------------------------------------
--
-- smart selectors/constructors for Expr-essions

address :: Prism' (TypedExpr v) Address
address = prism
  (\ i -> Node (Adr i) [])
  (\ case
      Node (Adr i) _xs -> Right i
      x                -> Left  x
  )

label :: Prism' (TypedExpr v) Label
label = prism
  (\ i -> Node (Lab i) [])
  (\ case
      Node (Lab i) _xs -> Right i
      x                -> Left  x
  )

lit :: Prism' (TypedExpr v) v
lit = prism
  (\ i -> Node (Lit i) [])
  (\ case
      Node (Lit i) _xs -> Right i
      x                -> Left  x
  )

expr :: Prism' (TypedExpr v) (Mnemonic, TypedExprs v)
expr = prism
  (\ (i, xs) -> Node (Opr i) xs)
  (\ case
      Node (Opr i) xs -> Right (i, xs)
      x               -> Left  x
  )

expr0 :: Prism' (TypedExpr v) Mnemonic
expr0 = prism
  (\ i -> Node (Opr i) [])
  (\ case
      Node (Opr i) [] -> Right i
      x               -> Left  x
  )

expr1 :: Prism' (TypedExpr v) (Mnemonic, TypedExpr v)
expr1 = prism
  (\ (i, x) -> Node (Opr i) [x])
  (\ case
      Node (Opr i) [x] -> Right (i, x)
      x                -> Left  x
  )

expr2 :: Prism' (TypedExpr v) (Mnemonic, (TypedExpr v, TypedExpr v))
expr2 = prism
  (\ (i, (x1, x2)) -> Node (Opr i) [x1, x2])
  (\ case
      Node (Opr i) [x1, x2] -> Right (i, (x1, x2))
      x                     -> Left  x
  )

expr3 :: Prism' (TypedExpr v) (Mnemonic, (TypedExpr v, TypedExpr v, TypedExpr v))
expr3 = prism
  (\ (i, (x1, x2, x3)) -> Node (Opr i) [x1, x2, x3])
  (\ case
      Node (Opr i) [x1, x2, x3] -> Right (i, (x1, x2, x3))
      x                         -> Left  x
  )

hasOp :: (Mnemonic -> Bool) -> Prism' (TypedExpr v) (TypedExpr v)
hasOp p = filtered p'
  where
    p' (Node (Opr i) _) = p i
    p' _                = False

-- ----------------------------------------
-}
