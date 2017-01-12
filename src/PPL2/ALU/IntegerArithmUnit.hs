module PPL2.ALU.IntegerArithmUnit where

import           PPL2.Prim.Prelude
import           PPL2.Prim.Values
import           PPL2.Control.Types
import           PPL2.ALU.MicroOperations

-- ----------------------------------------
--
-- a unit for integer arithmetic

integerArithmeticUnit :: WordValue v => [(String, MicroInstr v)]
integerArithmeticUnit =
  [ "incri"   |-> microInt'Int     (+ 1)      -- unary arithmetic
  , "decri"   |-> microInt'Int     (\ x -> x - 1)
  , "negi"    |-> microInt'Int     (\ x -> 0 - x)
  , "is0i"    |-> microInt'Bool    (== 0)

  , "addi"    |-> microIntInt'Int  (+)        -- binary arithmetic
  , "subi"    |-> microIntInt'Int  (-)
  , "muli"    |-> microIntInt'Int  (*)
  , "divi"    |-> microComp2 _Int intNE0 _Int div
  , "modi"    |-> microComp2 _Int intNE0 _Int mod

                                              -- div and mod with both results
  , "divmodi" |-> microComp2'2 _Int intNE0 _Int _Int divMod

  , "eqi"     |-> microIntInt'Bool (==)       -- binary predicates
  , "nei"     |-> microIntInt'Bool (/=)
  , "gei"     |-> microIntInt'Bool (>=)
  , "gri"     |-> microIntInt'Bool (>)
  , "lei"     |-> microIntInt'Bool (<=)
  , "lsi"     |-> microIntInt'Bool (<)
  ]
  where
    intNE0 :: WordValue v => Prism' v Int
    intNE0 = _Int . predP (/= 0)

-- ----------------------------------------
