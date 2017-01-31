{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}

module PPL2.CodeGen.TypedExpr where

import PPL2.Prelude
import PPL2.VM.Types

import Data.Tree
import Data.Tree.Lens
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
-- smart selectors/constructors and predicates for Typ-es

tName :: Lens' Typ Name
tName = root

tSubTyps :: Lens' Typ [Typ]
tSubTyps = branches

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

tTuple :: Prism' Typ [Typ]
tTuple = prism
  (\ ts -> (tVoid # ()) & tSubTyps .~ ts)
  (\ case
      Node "(,)" ts -> Right ts
      x             -> Left  x
  )

tSize :: Typ -> Int
tSize = length . tComp

tComp :: Typ -> [Typ]
tComp t
  | Just _ <- t ^? tVoid    = mempty
  | Just _  <- t ^? tSimple  = return t
  | sts     <- t ^. tSubTyps = sts >>= tComp

prettyTyp :: Typ -> String
prettyTyp t
  | Just ()    <- t ^? tVoid   = "()"
  | Just n     <- t ^? tSimple = n
  | Just "(,)" <- t ^? tName   =
                  "(" ++ intercalate "," (map prettyTyp $ t ^. tSubTyps) ++ ")"
  | otherwise                  = t ^. tName ++ " [" ++
                         intercalate "," (map prettyTyp $ t ^. tSubTyps) ++ "]"

instance Show Typ where show = prettyTyp

instance Monoid Typ where
  mempty = tVoid # ()
  t1 `mappend` t2
    -- one arg (), take the other
    | Just _ <- t1 ^? tVoid = t2
    | Just _ <- t2 ^? tVoid = t1

    -- both tuples, ++ subtype lists
    | Just ts1 <- t1 ^? tTuple
    , Just ts2 <- t2 ^? tTuple =
        tTuple # (ts1 ++ ts2)

    -- on a tuple, append or cons the other
    | Just ts1 <- t1 ^? tTuple =
        tTuple # (ts1 ++ [t2])

    | Just ts2 <- t2 ^? tTuple =
        tTuple # (t1 : ts2)

    -- else build a pair type
    | otherwise =
        tTuple # [t1, t2]


-- ----------------------------------------
--
-- lenses, prisms and traversals for typed expressions
-- type Traversal s t a b = Applicative f => (a -> f b) -> s -> f t
-- traverseFst ::           Applicative f => (t -> f a1) -> (t, a) -> f (a1, a)

exprl :: Lens' (TypedExpr v) [TypedExpr v]
exprl = branches

isoTreePair :: Iso' (Tree a) (a, [Tree a])
isoTreePair = iso (\ (Node i xs) -> (i, xs))  (uncurry Node)

prismListPair :: Prism' [a] (a, a)
prismListPair = prism
  (\ (x1, x2) -> [x1, x2])
  (\ case
      [x1, x2] -> Right (x1, x2)
      x        -> Left x
  )

prismList2 :: Prism' [a] [a]
prismList2 = prism
  id
  (\ case
      x@[_, _] -> Right x
      x        -> Left  x
  )

list2 :: Prism' [a] [a]
list2 = prism
  id
  (\ case
      x@[_, _] -> Right x
      x        -> Left  x
  )

list1 :: Prism' [a] a
list1 = prism
  (:[])
  (\ case
      [x] -> Right x
      x   -> Left  x
  )



-- just _1 and _2 !!!
traverse_1 :: Traversal (a, c) (b, c) a b
traverse_1 inj (x1, x2) = (,) <$> inj x1 <*> pure x2

traverse2 :: Traversal (c, a) (c, b) a b
traverse2 inj (x1, x2) = (,) x1 <$> inj x2


tOpr :: Prism' (TypedExOp v) Mnemonic
tOpr = prism
  TOpr
  (\ case
      TOpr n -> Right n
      x      -> Left  x
  )

-- select the argument type of a unary expression as a pattern guard
-- | Just t1 <- e ^? tType2

tTyp1 :: Traversal' (TypedExpr v) Typ
tTyp1 = exprl . list1 . root . _2

-- select the argument types of a binary expression as a pattern guard
-- | [t1,t2] <- e ^.. tType2

tTyp2 :: Traversal' (TypedExpr v) Typ
tTyp2 = exprl . list2 . traverse . root . _2

tOpr' :: Traversal' (TypedExpr v) (Mnemonic, Typ)
tOpr' = root . opr'
  where
    opr' = prism
      (\ (i, t) -> (TOpr i, t))
      (\ case
        (TOpr n, t) -> Right (n, t)
        x           -> Left  x
      )

tTyp :: Lens' (TypedExpr v) Typ
tTyp = root . _2

{- }

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
