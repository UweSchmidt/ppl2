module PPL2.VM.Pretty.Instr where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.VM.ALU.Types     (Mnemonics)
import PPL2.VM.Control.Types (MicroInstr)

-- ----------------------------------------

prettyAddr :: Address -> String
prettyAddr (AbsA i) = "m[" ++ show i ++ "]"
prettyAddr (LocA i) = "l[" ++ show i ++ "]"

prettyInstr :: (String -> String) ->
               (op  -> String) ->
               (lab -> [String]) ->
               (lab -> String) ->
               Instr op lab -> String
prettyInstr indent prettyOp prettyJmp prettyLab ins =
  case ins of
    Label lab -> prettyLab lab
    _         -> indent . fmt $ pretty'
  where
    pretty' :: [String]
    pretty' =
      case ins of
        Load  a    -> ["load     ", prettyAddr a]
        Store a    -> ["store    ", prettyAddr a]
        LoadInd    -> ["loadInd"]
        StoreInd   -> ["storeInd"]
        LoadI i    -> ["loadInt  ", show i]
        Pop        -> ["pop"]
        Dup        -> ["dup"]
        Swap       -> ["swap"]
        LoadAddr a -> ["loadAddr ", prettyAddr a]
        Br True  l ->  "brtrue   " : prettyJmp l
        Br False l ->  "brfalse  " : prettyJmp l
        Jump     l ->  "jump     " : prettyJmp l
        SRJump   l ->  "srjump   " : prettyJmp l
        LoadLab  l ->  "loadlab  " : prettyJmp l
        JumpInd    -> ["jumpind"]
        SRJumpInd  -> ["srjumpind"]
        Enter ub   -> ["pushframe", show $ ub + 1]
        Leave      -> ["popframe"]
        Comp op'   -> [prettyOp op']
        Term       -> ["terminate"]
        _          -> []
    fmt = unwords . align1

    align1 :: [String] -> [String]
    align1 (x1 : xs@(_ : _)) = fillRight 8 x1 : align2 xs
    align1 xs = xs

    align2 (x2 : xs@(_ : _)) = fillRight 8 x2 : xs
    align2 xs = xs

fillRight :: Int -> String -> String
fillRight n xs
  | length xs < n = take n (xs ++ replicate n ' ')
  | otherwise     = xs

fillLeft :: Int -> String -> String
fillLeft n xs
  | m > 0     = replicate m ' ' ++ xs
  | otherwise = xs
  where
    m = n - length xs

-- ----------------------------------------
--
-- trace an instruction during prog execution

instrTrc :: Mnemonics -> (String -> MicroInstr v) -> MInstr -> Offset -> MicroInstr v
instrTrc mns cmd ins pc' =
  cmd line
  where
    line = prettyInstr indent prettyOp prettyJmp prettyLab ins

    indent xs =
      fillLeft 6 (show pc') ++ ": " ++ xs

    prettyOp op' =
      fromMaybe ("undefinded-opcode-" ++ show op') . listToMaybe . drop op' $ mns

    prettyJmp disp =
      [show disp, "--> " ++ show (pc' + toEnum disp)]

    prettyLab disp =
      show disp ++ ":"

-- ----------------------------------------
