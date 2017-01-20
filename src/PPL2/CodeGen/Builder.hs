{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- haskell-mode wants this
{-# LANGUAGE LambdaCase #-}

module PPL2.CodeGen.Builder where

import PPL2.Prelude
import PPL2.VM.Types

-- ----------------------------------------

newtype Builder a = BU ([a] -> [a])

instance Monoid (Builder a) where
  mempty = BU id
  BU x `mappend` BU y = BU $ x . y


type Code = Builder AInstr

gi :: AInstr -> Code
gi i = BU (i:)

toACode :: Code -> ACode
toACode (BU f) = f []

-- ----------------------------------------

gLoad :: Address -> Code
gLoad = gi . Load

gComp :: Mnemonic -> Code
gComp = gi . Comp

gJump :: Label -> Code
gJump = gi . Jump

gLabel :: Label -> Code
gLabel = gi . Label

gBrFalse :: Label -> Code
gBrFalse = gi . Br False

gBrTrue :: Label -> Code
gBrTrue = gi . Br True

-- ----------------------------------------
