module PPL2.VM.ALU.IntegerArithmUnit where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.VM.ALU.MicroOps
import PPL2.VM.ALU.Types     (CompInstrSet)

-- ----------------------------------------
--
-- a unit for integer arithmetic

integerArithmeticUnit :: WordValue v => CompInstrSet v
integerArithmeticUnit =
  [ "incri"   .-> microInt'Int     (+ 1)      -- unary arithmetic
  , "decri"   .-> microInt'Int     (\ x -> x - 1)
  , "negi"    .-> microInt'Int     (\ x -> 0 - x)
  , "nulli"   .-> microInt'Bool    (== 0)

  , "addi"    .-> microIntInt'Int  (+)        -- binary arithmetic
  , "subi"    .-> microIntInt'Int  (-)
  , "muli"    .-> microIntInt'Int  (*)
  , "divi"    .-> microComp2 _Int intNE0 _Int div
  , "modi"    .-> microComp2 _Int intNE0 _Int mod

                                              -- div and mod with both results
  , "divmodi" .-> microComp2'2 _Int intNE0 _Int _Int divMod

  , "eqi"     .-> microIntInt'Bool (==)       -- binary predicates
  , "nei"     .-> microIntInt'Bool (/=)
  , "gei"     .-> microIntInt'Bool (>=)
  , "gri"     .-> microIntInt'Bool (>)
  , "lei"     .-> microIntInt'Bool (<=)
  , "lsi"     .-> microIntInt'Bool (<)
  ]
  where
    intNE0 :: WordValue v => Prism' v Int
    intNE0 = _Int . prismPred (/= 0)

-- ----------------------------------------
