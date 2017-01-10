-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PPL2.MicroInstructions where

import           PPL2.Prim.Prelude
import           PPL2.Prim.Values
import           PPL2.Prim.Instr

import qualified PPL2.Memory.RTS     as RTS
import qualified PPL2.Memory.Segment as Segment
import qualified PPL2.Memory.Stack   as Stack
import qualified PPL2.Memory.CodeSeg as CodeSeg
import           PPL2.Memory.State

import           PPL2.Control.Monad

import           PPL2.Pretty.Instr

import Control.Monad.Except

import System.IO

-- ----------------------------------------

type ALU v = OpCode -> Maybe (String, (Int, Int), [v] -> MicroCode v [v])

-- ----------------------------------------

check' :: MStatus v -> Maybe a -> MicroCode v a
check' exc mv = maybe (abort exc) return mv

checkValue :: (v -> MStatus v) -> Prism' v a -> v -> MicroCode v a
checkValue exc pr v = check' (exc v) (preview pr v)

checkCodeRef' :: CodeRefValue v => CodeRef -> Maybe a -> MicroCode v a
checkCodeRef' i mv = check' (AddressViolation $ _CodeRef # i) mv

checkDataRef' :: DataRefValue v => DataRef -> Maybe a -> MicroCode v a
checkDataRef' r mv = check' (AddressViolation $ _DataRef # r) mv
{- }
check :: MStatus v -> MicroCode v (Maybe a) -> MicroCode v a
check exc cmd =
  cmd >>= maybe (abort exc) return
-- -}
toValue :: Prism' v a -> v -> MicroCode v a
toValue = checkValue IllegalArgument

toDataRef :: DataRefValue v => v -> MicroCode v (SegId, Offset)
toDataRef = toValue _DataRef

toCodeRef :: CodeRefValue v => v -> MicroCode v Offset
toCodeRef = toValue _CodeRef

-- ----------------------------------------

getInstr :: CodeRefValue v => CodeRef -> MicroCode v MInstr
getInstr i = getInstr' >>= checkCodeRef' i
  where
    getInstr' = CodeSeg.get i <$> use msInstr

getPC :: MicroCode v Offset
getPC = use msPC

setPC :: Offset -> MicroInstr v
setPC = (msPC .=)

incrPC :: MicroInstr v
incrPC = modPC 1

modPC :: Int -> MicroInstr v
modPC disp = msPC += toEnum disp

-- ----------------------------------------

-- address to DataRef without validation
-- used for load and store

address2ref :: Address -> MicroCode v DataRef
address2ref (AbsA i) = Segment.toDataRef i <$> use msMem
address2ref (LocA i) =     RTS.toDataRef i <$> use msFrames

-- address to DataRef with validation
-- used for load effective address
-- validation of address is done by reading the cell (getByDataRef)
-- and discarding the value

address2ref' :: DataRefValue v => Address -> MicroCode v DataRef
address2ref' a = do
  r <- address2ref a
  getByDataRef r >> return r

getByAddress :: DataRefValue v => Address -> MicroCode v v
getByAddress = address2ref >=> getByDataRef

getByDataRef :: DataRefValue v => DataRef -> MicroCode v v
getByDataRef r@(sid, i) = getByDataRef' >>= checkDataRef' r
  where
    getByDataRef' :: MicroCode v (Maybe v)
    getByDataRef'
      | sid == dataSid = Segment.get  i <$> use msMem
      | otherwise      = RTS.get  sid i <$> use msFrames

-- ----------------------------------------

putByAddress :: DataRefValue v => Address -> v -> MicroInstr v
putByAddress a v = do
  address2ref a >>= flip putByDataRef v

putByDataRef :: DataRefValue v => DataRef -> v -> MicroInstr v
putByDataRef r@(sid, i) v
  | sid == dataSid =
      (Segment.put i v <$> use msMem)  -- get mem and modify it
      >>= checkDataRef' r              -- check success of modification
      >>= (msMem .=)                   -- store the segment

  | otherwise = do
      rts' <- (RTS.put sid i v <$> use msFrames) >>= checkDataRef' r
      msFrames .= rts'

-- ----------------------------------------

push :: v -> MicroInstr v
push mv = msStack %= Stack.push mv

pop :: MicroCode v v
pop = do
  (v, s')  <- uses msStack Stack.get >>= check' EvalStackUnderflow
  msStack .= s'
  return v

-- ----------------------------------------

iLoad :: DataRefValue v => Address -> MicroInstr v
iLoad a = getByAddress a >>= push

iStore :: DataRefValue v => Address -> MicroInstr v
iStore a = pop >>= putByAddress a

iLoadInd :: DataRefValue v => MicroInstr v
iLoadInd = pop >>= toDataRef >>= getByDataRef >>= push

iStoreInd :: DataRefValue v => MicroInstr v
iStoreInd = do
  v <- pop
  pop >>= toDataRef >>= flip putByDataRef v

iLoadI :: WordValue v => Int -> MicroInstr v
iLoadI i = push $ _Int # i

iPop :: MicroInstr v
iPop = pop >> return ()

iDup :: MicroInstr v
iDup = do
  v <- pop
  push v
  push v

iSwap :: MicroInstr v
iSwap = do
  v1 <- pop
  v2 <- pop
  push v1
  push v2

iLoadAddr :: DataRefValue v => Address -> MicroInstr v
iLoadAddr a =
  ((_DataRef #) <$> address2ref' a) >>= push

iBr :: WordValue v => Bool -> Displ -> MicroInstr v
iBr b disp = do
  v <- pop >>= toValue _Bool
  when (b == v) $
    modPC (disp - 1)  -- pc already incremented

iJump :: CodeRefValue v => Displ -> MicroInstr v
iJump disp = modPC (disp - 1)

iSRJump :: CodeRefValue v => Displ -> MicroInstr v
iSRJump disp = do
  ((_CodeRef #) <$> getPC) >>= push
  modPC (disp - 1)

iLoadLab :: CodeRefValue v => Displ -> MicroInstr v
iLoadLab disp = do
  i <- (+ toEnum (disp - 1)) <$> getPC
  getInstr i >> push (_CodeRef # i)

iJumpInd :: CodeRefValue v => MicroInstr v
iJumpInd = pop >>= toCodeRef >>= setPC

iSRJumpInd :: CodeRefValue v => MicroInstr v
iSRJumpInd = do
  i <- pop >>= toCodeRef
  (review _CodeRef <$> getPC) >>= push
  setPC i

iEnter :: DefaultValue v => Offset -> MicroInstr v
iEnter ub =
  msFrames %= RTS.push newFrame
  where
    newFrame = Segment.new ub (_Default # ())

iLeave :: MicroInstr v
iLeave = do
  (RTS.pop <$> use msFrames)
    >>= check' RTStackUnderflow
    >>= (msFrames .=)

iTerm :: MicroInstr v
iTerm = abort Terminated

iLabel :: MicroInstr v
iLabel = return ()

-- ----------------------------------------
--
-- the working horse

iComp :: ALU v -> OpCode -> MicroInstr v
iComp alu opc = do
  (_name, (noArgs, noRes), evalfct) <- check' IllegalOpCode (alu opc)
  args <- getArgs noArgs
  res  <- evalfct args
  when (length res /= noRes) $
    abort IllegalResult
  mapM_ push res
  where
    getArgs 0 = return []
    getArgs n
      | n <= 0 =
          return []
      | otherwise = do
          rs <- getArgs (n -1)
          r  <- pop
          return (r : rs)

-- ----------------------------------------
--

type ArithmUnit v = [(Mnemonic, MicroInstr v)]

-- ----------------------------------------
--
-- a unit for integer arithmetic

integerArithmeticUnit :: WordValue v => [(String, MicroInstr v)]
integerArithmeticUnit =
  [ "incri" |-> microInt'Int     (+ 1)      -- unary arithmetic
  , "decri" |-> microInt'Int     (\ x -> x - 1)
  , "negi"  |-> microInt'Int     (\ x -> 0 - x)
  , "is0i"  |-> microInt'Bool    (== 0)

  , "addi"  |-> microIntInt'Int  (+)        -- binary arithmetic
  , "subi"  |-> microIntInt'Int  (-)
  , "muli"  |-> microIntInt'Int  (*)
  , "divi"  |-> microInstr2 _Int intNE0 _Int div
  , "modi"  |-> microInstr2 _Int intNE0 _Int mod

  , "eqi"   |-> microIntInt'Bool (==)       -- binary predicates
  , "nei"   |-> microIntInt'Bool (/=)
  , "gei"   |-> microIntInt'Bool (>=)
  , "gri"   |-> microIntInt'Bool (>)
  , "lei"   |-> microIntInt'Bool (<=)
  , "lsi"   |-> microIntInt'Bool (<)
  ]
  where
    intNE0 :: WordValue v => Prism' v Int
    intNE0 = _Int . predP (/= 0)

-- ----------------------------------------
--
-- lift functions to micro instructions

pushMV :: Prism' v a -> a -> MicroInstr v
pushMV pa v = push (pa # v)

popMV :: Prism' v a -> MicroCode v a
popMV p = pop >>= toValue p

microInstr1 :: Prism' v a ->
               Prism' v b ->
               (a -> b) ->
               MicroInstr v

microInstr1 pa pb oc = do
  res <- oc <$> popMV pa
  pushMV pb res

microInt'Int  :: WordValue v => (Int -> Int ) -> MicroInstr v
microInt'Bool :: WordValue v => (Int -> Bool) -> MicroInstr v

microInt'Int  = microInstr1 _Int _Int
microInt'Bool = microInstr1 _Int _Bool

microInstr2 :: Prism' v a ->
               Prism' v b ->
               Prism' v c ->
               (a -> b -> c) ->
               MicroInstr v

microInstr2 pa pb pc oc = do     -- !!! last operand is on top of stack
  res <- flip oc <$> popMV pb <*> popMV pa
  pushMV pc res

microIntInt'Int  :: WordValue v => (Int -> Int -> Int ) -> MicroInstr v
microIntInt'Bool :: WordValue v => (Int -> Int -> Bool) -> MicroInstr v

microIntInt'Int  = microInstr2 _Int _Int _Int
microIntInt'Bool = microInstr2 _Int _Int _Bool

-- ----------------------------------------

instrTrc :: ALU v -> MInstr -> Offset -> MicroInstr v
instrTrc alu ins pc' =
  io $ hPutStrLn stderr line
  where
    line = prettyInstr indent prettyOp prettyJmp prettyLab ins

    indent xs =
      fillLeft 6 (show pc') ++ ": " ++ xs

    prettyOp op' =
      maybe ("not-used-" ++ show op') (^. _1) $ alu op'

    prettyJmp disp =
      [show disp, "--> " ++ show (pc' + toEnum disp)]

    prettyLab disp =
      show disp ++ ":"

-- ----------------------------------------

runCPU :: (CodeRefValue v, DataRefValue v, DefaultValue v, WordValue v) => Bool -> ALU v -> MicroInstr v
runCPU trc alu = go
  where
    go = do
      continue <- statusOk <$> use msStatus
      when continue $ do
        pc    <- getPC
        instr <- getInstr pc
        incrPC

        -- trace the instructions
        when trc $ instrTrc alu instr pc

        case instr of
          Load  a    -> iLoad a
          Store a    -> iStore a
          Comp oc    -> iComp alu oc
          LoadInd    -> iLoadInd
          StoreInd   -> iStoreInd
          LoadI i    -> iLoadI i
          Pop        -> iPop
          Dup        -> iDup
          Swap       -> iSwap
          LoadAddr a -> iLoadAddr a
          Br b    t  -> iBr b t
          Jump    t  -> iJump t
          SRJump  t  -> iSRJump t
          LoadLab t  -> iLoadLab t
          JumpInd    -> iJumpInd
          SRJumpInd  -> iSRJumpInd
          Enter ub   -> iEnter ub
          Leave      -> iLeave
          Term       -> iTerm
          Label _    -> iLabel
        go

initMemory :: [MInstr] -> [v] -> MicroInstr v
initMemory is vs = do
  msInstr .= CodeSeg.new is
  setPC 0
  msMem    .= Segment.newInit vs
  msStack  .= Stack.new
  msFrames .= RTS.new
  msStatus .= Ok

-- ----------------------------------------
