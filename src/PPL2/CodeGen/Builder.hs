{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- haskell-mode wants this
{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.Builder where

import PPL2.Prelude
import PPL2.VM.Types

-- ----------------------------------------

newtype Builder a = BU {unBU :: ([a] -> [a])}

instance Monoid (Builder a) where
  mempty = BU id
  BU x `mappend` BU y = BU $ x . y

builder2List :: Builder a -> [a]
builder2List = ($ []) . unBU

-- ----------------------------------------

type Code = Builder AInstr

gi :: AInstr -> Code
gi i = BU (i:)

toACode :: Code -> ACode
toACode = builder2List

-- ----------------------------------------

gLoad :: Address -> Code
gLoad = gi . Load

gLoadInt :: Int -> Code
gLoadInt = gi . LoadI

gLoadInd :: Code
gLoadInd = gi LoadInd

gLoadLab :: Label -> Code
gLoadLab = gi . LoadLab

gStore :: Address -> Code
gStore = gi . Store

gStoreInd :: Code
gStoreInd = gi StoreInd

gPop :: Code
gPop = gi Pop

gDup :: Offset -> Code
gDup = gi . Dup

gComp :: Mnemonic -> Code
gComp = gi . Comp

gJump :: Label -> Code
gJump = gi . Jump

gLabel :: Label -> Code
gLabel = gi . Label

gSRJump :: Label -> Code
gSRJump = gi . SRJump

gSRJumpInd :: Code
gSRJumpInd = gi SRJumpInd

gBr :: Bool -> Label -> Code
gBr b = gi . Br b
{-
gBrFalse :: Label -> Code
gBrFalse = gi . Br False

gBrTrue :: Label -> Code
gBrTrue = gi . Br True
-- -}
gEnter :: Offset -> Code
gEnter = gi . Enter

gLeave :: Code
gLeave = gi Leave

gTerminate :: Code
gTerminate = gi Term

-- ----------------------------------------
