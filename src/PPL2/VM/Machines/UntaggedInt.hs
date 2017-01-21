{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instances for Word
{-# LANGUAGE TypeSynonymInstances #-}

-- this is a VM with simple Word values as machine values
-- and just simple integer arithmetic
--
-- no type checking, no security against illegal usage of
-- Int's as code or data pointers is detected

-- ----------------------------------------

module PPL2.VM.Machines.UntaggedInt where

import PPL2.Prelude
import PPL2.VM
import PPL2.VM.ALU.IntegerArithmUnit
import PPL2.CodeGen

import Data.Bits (shiftR, shiftL, (.|.), (.&.))

-- ----------------------------------------

type MLit    = Int

type MExpr   = Expr Int

instance LoadLit MLit where
  gLoadLit i = return $ gLoadInt i

genACode :: MExpr -> ACode
genACode e =
  either (const []) toACode res
  where
    (res, _gcs1) = runGC $ genCode (toMnemonics instrSet) e

-- ----------------------------------------

type MV      = Word

type MCode a = MicroCode MV a

-- ----------------------------------------
--
-- basic access prisms

instance WordValue MV where
  _Word = prism id Right

instance DataRefValue MV where
  _DataRef = _Word . isoWordRef
    where
      isoWordRef = iso toRef frRef

      toRef w =
        (fromEnum w `shiftR` 32, w .&. 0xffffffff)

      frRef (sid, i) =
        toEnum sid `shiftL` 32 .|. i

instance CodeRefValue MV where
  _CodeRef = _Word

instance DefaultValue MV where
  _Default = prism
             (const 0)
             (\ w -> if w == 0 then Right () else Left w)

-- ----------------------------------------

instrSet :: CInstrSet MV
instrSet = integerArithmeticUnit

execProg :: Bool -> [MInstr] -> [MV] -> IO (MState MV)
execProg = execProg' instrSet

-- ----------------------------------------
