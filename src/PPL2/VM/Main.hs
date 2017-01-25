module PPL2.VM.Main where

import PPL2.Prelude
import PPL2.VM.Types

import qualified PPL2.VM.Memory.CodeSeg       as CodeSeg
import qualified PPL2.VM.Memory.Segment       as Segment
import           PPL2.VM.Memory.State         (MState, newMState
                                              ,msInstr, msMem)
import           PPL2.VM.Control.Types        (runMicroCode)
import           PPL2.VM.Control.Loop
import           PPL2.VM.ALU.Types

import           PPL2.System.Types

-- ----------------------------------------

execute :: (MonadCompile m, CoreValue v) =>
           CInstrSet v -> Bool -> MProg v -> m (MState v)
execute inset trc (mcode, mdata) =  do
  s1 <- liftIO $ runMicroCode (initMem >> execLoop trcOutput inset) newMState
  return $ snd s1
  where
    initMem = do
      msInstr .= CodeSeg.new     mcode
      msMem   .= Segment.newInit mdata
    trcOutput
      | trc       = tostderr
      | otherwise = todevnull

-- ----------------------------------------
