{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module PPL2.ALU.MicroOperations where

import           PPL2.Prim.Prelude
import           PPL2.Prim.Values

import           PPL2.Control.MicroOperations
import           PPL2.Control.Types

-- ----------------------------------------
--
-- lift for unary compute operations

microComp1 :: Prism' v a ->
              Prism' v b ->
              (a -> b) ->
              MicroInstr v

microComp1 pa pb oc = do
  res <- oc <$> popMV pa
  pushMV pb res

microInt'Int  :: WordValue v => (Int -> Int ) -> MicroInstr v
microInt'Int  = microComp1 _Int _Int

microInt'Bool :: WordValue v => (Int -> Bool) -> MicroInstr v
microInt'Bool = microComp1 _Int _Bool

-- ----------------------------------------
--
-- lift for binary compute operations

microComp2 :: Prism' v a ->
              Prism' v b ->
              Prism' v c ->
              (a -> b -> c) ->
              MicroInstr v

microComp2 pa pb pc oc = do     -- !!! last operand is on top of stack
  res <- flip oc <$> popMV pb <*> popMV pa
  pushMV pc res

microIntInt'Int  :: WordValue v => (Int -> Int -> Int ) -> MicroInstr v
microIntInt'Int  = microComp2 _Int _Int _Int

microIntInt'Bool :: WordValue v => (Int -> Int -> Bool) -> MicroInstr v
microIntInt'Bool = microComp2 _Int _Int _Bool

-- ----------------------------------------
--
-- lift for binary compute operations with 2 results

microComp2'2 :: Prism' v a ->
                Prism' v b ->
                Prism' v c ->
                Prism' v d ->
                (a -> b -> (c, d)) ->
                MicroInstr v

microComp2'2 pa pb pc pd oc = do     -- !!! last operand is on top of stack
  (res1, res2) <- flip oc <$> popMV pb <*> popMV pa
  pushMV pc res1
  pushMV pd res2

-- ----------------------------------------
