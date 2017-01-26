module Main where

import PPL2.Prelude
import PPL2.VM
import PPL2.CodeGen
import PPL2.Assemble.Assemble (assembleCode)

import PPL2.Pretty.Instr
import PPL2.Pretty.AProg
import PPL2.Pretty.MProg
import PPL2.Pretty.MState
import PPL2.Pretty.UntypedExpr

import PPL2.CodeGen.Main
import PPL2.Assemble.Main
import PPL2.VM.Main
import PPL2.System.Types

import qualified PPL2.VM.Machines.UntaggedInt as U
import qualified PPL2.VM.Machines.TaggedInt   as T

import           PPL2.VM.Memory.State
import qualified PPL2.VM.Memory.Segment as Segment


genCode' :: UntypedExpr U.MV -> RunCompile (ACode, [U.MV])
genCode' = genCode (hasOpCode U.instrSet)

e1 :: CoreValue v => UntypedExpr v
e1 = expr # (",", [glb, xy12, swap, loop, cndst, ifst, ifex, asss])
  where
    glb    = expr1 # ("globals", lit . _Word # 10) -- allocate 10 global variables
    xy12   = expr2 # (":=", (xy, i0 .+. i2))
    swap   = expr2 # (":=", (xy, yx))
    loop   = while (x `nei` y)
                   (xy `ass` (x `subi` i1 .+. y `addi` i1))
    cndst  = ifthen ( x `eqi` i42) (x `ass` (x `addi` i42))
    ifst   = ifthenelse (y `gei` i1) ( y `ass` i42) (y `ass` i23)
    ifex   = z `ass` ifthenelse (y `gei` i0) y (negi y)
    asss   = voidex (x `cpy` (y `cpy` (z `cpy` i23)))

    xy     = x .+. y
    yx     = y .+. x
    i1i2   = i1 .+. i2

    x      = address # AbsA 1
    y      = address # AbsA 2
    z      = address # AbsA 3
    u      = address # AbsA 4
    v      = address # AbsA 5

    -- i0, i1, i42 :: CoreValue v => UntypedExpr v
    i0     = lit . _Int # 0
    i1     = lit . _Int # 1
    i2     = lit . _Int # 2
    i23    = lit . _Int # (23::Int)
    i42    = lit . _Int # (42::Int)

-- ass, while, ifthen, eqi, addi, subi :: UntypedExpr v -> UntypedExpr v -> UntypedExpr v
void'      = expr0 # "void"
voidex e   = void' `ass` e
ass    l r = expr2 # (":=", (l, r))
cpy    l r = expr2 # ("copy", (l, r))
while  c b = expr2 # ("while", (c, b))
ifthen c t = expr2 # ("if", (c, t))
ifthenelse c t e = expr3 # ("if", (c, t, e))
eqi  e1 e2 = expr2 # ("eqi", (e1, e2))
nei  e1 e2 = expr2 # ("nei", (e1, e2))
gei  e1 e2 = expr2 # ("gei", (e1, e2))
addi e1 e2 = expr2 # ("addi", (e1, e2))
subi e1 e2 = expr2 # ("subi", (e1, e2))
negi e1    = expr1 # ("negi", e1)


infixr 6 .+.
(.+.) e1 e2 = expr2 # (",", (e1, e2))


p1' :: ACode
p1' =
  [ Load (AbsA 0)
  , Load (AbsA 1)
  , Comp "addi"
  , Store (AbsA 2)
  , Load (AbsA 0)
  , Load (AbsA 1)
  , Comp "eqi"
  , Br True "l1"
  , LoadI 42
  , Store (AbsA 3)
  , Jump "l2"
  , Label "l1"
  , Load (AbsA 0)
  , Load (AbsA 1)
  , Comp "maxi"
  , Store (AbsA 3)
  , Label "l2"
  , LoadAddr (AbsA 1)
  , LoadInd
  , LoadAddr (AbsA 4)
  -- , Swap  -- no swap, address is on top of stack, the value below
  , StoreInd
  , Term
  ]

m1' :: CoreValue v => [v]
m1' =
  [ _Int # 1
  , _Int # 2
  , _Default # ()
  , _Default # ()
  , _Default # ()
  ]

p1 :: MCode
p1 = snd $ assembleCode (toOpCode U.instrSet) p1'

m1 :: [U.MV]
m1 = m1'

p2 :: MCode
(e2, p2) = assembleCode (toOpCode T.instrSet) p1'

m2 :: [T.MV]
m2 = m1'

compilePipeline :: (Show v, CoreValue v) =>
                    CInstrSet v -> UntypedExpr v -> RunCompile ()
compilePipeline instrSet =
  tee (tostderr . prettyUntypedExpr show)
  >=>
  genCode (hasOpCode instrSet)
  >=>
  stopWhen onlyGenCode
  >=>
  tee (tostderr . prettyAProg)
  >=>
  assemble (toOpCode instrSet)
  >=>
  stopWhen onlyAssemble
  >=>
  tee (tostderr . prettyMProg (toMnemonic instrSet))
  >=>
  execute instrSet True
  >=>
  tee (tostderr . prettyMState)
  >=>
  checkFinalState
  >=>
  stopWhen success
  >=>
  (const $ return ())

main1 :: IO ()
main1 = execCompile $ compilePipeline U.instrSet e1


main :: IO ()
main = do
  putStrLn "assembler code"
  putStrLn $ unlines $ prettyACode p1'

  let (es, p1) = assembleCode (toOpCode U.instrSet) p1'
  unless (null es) $ do
     die $ unlines ("assembly errors:" : es)

  putStrLn "machine code"
  putStrLn $ unlines $ prettyMCode (toMnemonic U.instrSet) p1

  putStrLn "exec program"
  (MS instr pc stack mem frames status) <-
    either undefined id <$> (runCompile $ execute U.instrSet True (p1, m1))

  putStrLn $ "\nmachine status register: " ++ show status

  putStrLn   "\nglobal memory dump"
  print $ Segment.dump mem

  return ()
