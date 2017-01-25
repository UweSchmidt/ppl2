module PPL2.Pretty.MState where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.VM.Memory.State
import PPL2.Pretty.Instr

import PPL2.VM.Memory.RTS     (RTS)
import PPL2.VM.Memory.Segment (Segment)
import PPL2.VM.Memory.Stack   (Stack)
import PPL2.VM.Memory.CodeSeg (CodeSegment)

import qualified PPL2.VM.Memory.RTS     as RTS
import qualified PPL2.VM.Memory.Segment as Segment
import qualified PPL2.VM.Memory.Stack   as Stack
import qualified PPL2.VM.Memory.CodeSeg as CodeSeg

import PPL2.CodeGen.Builder (Builder(..),builder2List)

type Lines = Builder String

ln :: String -> Lines
ln l = BU (l:)

nl = ln ""

prettyMState :: (Show v) => MState v -> String
prettyMState = unlines . builder2List . prettyMState'

prettyMState' :: (Show v) => MState v -> Lines
prettyMState' s = mconcat $
  [ nl
  , ln "machine state"
  , ln "============="
  , nl
  , ln "status register"
  , ln "==============="
  , nl
  , prettyMStatus $ s ^. msStatus
  , nl
  , ln "program counter"
  , ln "==============="
  , nl
  , prettyPC $ s ^. msPC
  , nl
  , ln "evaluation stack"
  , ln "================"
  , nl
  , prettyStack $ s ^. msStack
  , nl
  , ln "global memory"
  , ln "============="
  , nl
  , prettySegment $ s ^. msMem
  , nl
  , ln "runtime stack"
  , ln "============="
  , nl
  , prettyRTS $ s ^. msFrames
  , nl
  ]

prettyMStatus :: Show v => MStatus v -> Lines
prettyMStatus s = ln $ fmt ["status:", show s]

prettyPC :: CodeRef -> Lines
prettyPC pc' = ln $ fmt ["pc: ", show pc']

prettyStack :: Show v => Stack v -> Lines
prettyStack s
  | null s' =
      ln "<empty>"
  | otherwise =
      mconcat $ zipWith cell [0::Int ..] s'
  where
    cell i v =
      ln $ fmt [show i ++ ":", show v]

    s' = Stack.unStack s


prettySegment :: Show v => Segment v -> Lines
prettySegment =
  mconcat . zipWith cell [0..] . Segment.dump
  where
    cell i v = ln $ fmt [show i ++ ":", show v]

prettyRTS :: Show v => RTS v -> Lines
prettyRTS rts = ln "RTS dump not yet implemented"
