module PPL2.Assemble.Main
where

import PPL2.VM
import PPL2.Assemble.Assemble (assembleCode)
import PPL2.System.Types

-- ----------------------------------------
--
-- the main entry point into codegen

assemble :: (MonadCompile m) =>
            (Mnemonic -> Maybe OpCode) -> (ACode, [v]) -> m (MCode, [v])
assemble opc  (acode, adata)=
  either (issueError 23) (\ mcode -> return (mcode, adata)) $
  mapErr $
  assembleCode opc acode
  where
    mapErr ([], mcode) =
      Right mcode
    mapErr (es, _) =
      Left $ unlines $ "error(s) in assembling code" : es

-- ----------------------------------------
