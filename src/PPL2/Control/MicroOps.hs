{-# LANGUAGE RankNTypes #-} -- haskell mode wants this

module PPL2.Control.MicroOps where

import           PPL2.Prim.Prelude
import           PPL2.Prim.Values
import           PPL2.Prim.Instr     (MInstr, Address(..))

import qualified PPL2.Memory.RTS     as RTS
import qualified PPL2.Memory.Segment as Segment
import qualified PPL2.Memory.Stack   as Stack
import qualified PPL2.Memory.CodeSeg as CodeSeg
import           PPL2.Memory.State   (MStatus(..)
                                     ,msInstr, msPC, msMem, msFrames, msStack)

import           PPL2.Control.Types  (MicroCode, MicroInstr, abort)

-- ----------------------------------------
--
-- check values and raise exceptions

check' :: MStatus v -> Maybe a -> MicroCode v a
check' exc = maybe (abort exc) return

checkValue :: (v -> MStatus v) -> Prism' v a -> v -> MicroCode v a
checkValue exc pr v = check' (exc v) (preview pr v)

checkCodeRef' :: CodeRefValue v => CodeRef -> Maybe a -> MicroCode v a
checkCodeRef' i = check' (AddressViolation $ _CodeRef # i)

checkDataRef' :: DataRefValue v => DataRef -> Maybe a -> MicroCode v a
checkDataRef' r = check' (AddressViolation $ _DataRef # r)

toValue :: Prism' v a -> v -> MicroCode v a
toValue = checkValue IllegalArgument

toDataRef :: DataRefValue v => v -> MicroCode v (SegId, Offset)
toDataRef = toValue _DataRef

toCodeRef :: CodeRefValue v => v -> MicroCode v Offset
toCodeRef = toValue _CodeRef

-- ----------------------------------------
--
-- program counter access

getInstr :: CodeRefValue v => CodeRef -> MicroCode v MInstr
getInstr i =
  (CodeSeg.get i <$> use msInstr)
  >>= checkCodeRef' i

getPC :: MicroCode v Offset
getPC = use msPC

setPC :: Offset -> MicroInstr v
setPC = (msPC .=)

incrPC :: MicroInstr v
incrPC = modPC 1

modPC :: Int -> MicroInstr v
modPC disp = msPC += toEnum disp

-- ----------------------------------------
--
-- memory access and address calculation


-- address to DataRef conversion without validation
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

putByAddress :: DataRefValue v => Address -> v -> MicroInstr v
putByAddress a v =
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
--
-- evaluation stack access

push :: v -> MicroInstr v
push mv = msStack %= Stack.push mv

pop :: MicroCode v v
pop = do
  (v, s') <- uses msStack Stack.get
             >>= check' EvalStackUnderflow
  msStack .= s'
  return v

pushMV :: Prism' v a -> a -> MicroInstr v
pushMV pa v = push (pa # v)

popMV :: Prism' v a -> MicroCode v a
popMV p = pop >>= toValue p

-- ----------------------------------------
