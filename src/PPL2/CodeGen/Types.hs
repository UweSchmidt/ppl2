{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- haskell-mode wants this
{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.Types where

import PPL2.Prelude
import PPL2.VM.Types

import Data.Tree

-- ----------------------------------------

import Control.Applicative
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError
                            ,MonadIO, liftIO)
import Control.Monad.State  -- (State(..), MonadState)

-- ----------------------------------------

newtype GenCode v a
  = GC { unGC :: ExceptT (GCError v) (State GCState) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GCState
           , MonadError (GCError v)
           )

data GCError v
  = GCE String (Maybe (Expr v))
    deriving Show

data GCState
  = GCS { _labCnt :: Int
        }
    deriving Show

runGC :: GenCode v a -> GCState -> (Either (GCError v) a, GCState)
runGC = runState . runExceptT . unGC

runGC' :: GenCode Int a -> GCState -> (Either (GCError Int) a, GCState)
runGC' = runGC

labCnt :: Lens' GCState Int
labCnt k s = (\ new -> s {_labCnt = new}) <$> k (_labCnt s)

errGCExpr :: Expr v -> GCError v
errGCExpr = GCE "can't gencode" . Just

newLabel :: GenCode v Label
newLabel = do
  i <- labCnt <+= 1
  return $ 'L' : show i

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

newtype Builder a = BU ([a] -> [a])

instance Monoid (Builder a) where
  mempty = BU id
  BU x `mappend` BU y = BU $ x . y


type Code = Builder AInstr

gi :: AInstr -> Code
gi i = BU (i:)

toACode :: Code -> ACode
toACode (BU f) = f []

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
          throwError $ errGCExpr e

-- ----------------------------------------

class LoadLit v where
  gLoadLit :: v -> GenCode v Code

gLoad :: Address -> Code
gLoad = gi . Load

gComp :: Mnemonic -> Code
gComp = gi . Comp

gJump :: Label -> Code
gJump = gi . Jump

gLabel :: Label -> Code
gLabel = gi . Label

gBrFalse :: Label -> Code
gBrFalse = gi . Br False

gBrTrue :: Label -> Code
gBrTrue = gi . Br True

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
