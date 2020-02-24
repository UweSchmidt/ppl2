module PPL2.Pretty.Instr where

import PPL2.Prelude
import PPL2.VM.Types
import PPL2.VM.Control.Types (MicroInstr)

-- ----------------------------------------

prettyAddr :: Address -> String
prettyAddr (AbsA i) = "g[" ++ show i ++ "]"
prettyAddr (LocA i) = "l[" ++ show i ++ "]"

prettyInstr :: (String -> String) ->
               (op  -> String) ->
               (lab -> [String]) ->
               (lab -> String) ->
               Instr op lab -> String
prettyInstr indent' prettyOp' prettyJmp' prettyLab' ins =
  case ins of
    Label lab -> prettyLab' lab
    _         -> indent' . fmt $ pretty'
  where
    pretty' :: [String]
    pretty' =
      case ins of
        Load  a           -> ["load",  prettyAddr a]
        Store a           -> ["store", prettyAddr a]
        LoadInd           -> ["loadInd"]
        StoreInd          -> ["storeInd"]
        LoadI i           -> ["loadInt",  show i]
        Pop               -> ["pop"]
        Dup   i           -> ["dup",      show i]
        Swap              -> ["swap"]
        LoadAddr (AbsA a) -> ["loadAddrG", show a]
        LoadAddr (LocA a) -> ["loadAddrL", show a]
        Br True  l        ->  "brTrue"    : prettyJmp' l
        Br False l        ->  "brFalse"   : prettyJmp' l
        Jump     l        ->  "jump"      : prettyJmp' l
        SRJump   l        ->  "srjump"    : prettyJmp' l
        LoadLab  l        ->  "loadlab"   : prettyJmp' l
        JumpInd           -> ["jumpind"]
        SRJumpInd         -> ["srjumpind"]
        Enter ub          -> ["pushframe", show $ ub + 1]
        Leave             -> ["popframe"]
        Comp op'          -> [prettyOp' op']
        Term              -> ["terminate"]
        _                 -> []

fmt :: [String] -> String
fmt = unwords . align1

align1 :: [String] -> [String]
align1 (x1 : xs@(_ : _)) = fillRight 10 x1 : align2 xs
align1 (x1 : xs) = fillRight 10 x1 : xs
align1 xs = xs

align2 :: [String] -> [String]
align2 (x2 : xs@(_ : _)) = fillRight 10 x2 : xs
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

fmt' :: [String] -> String
fmt' = unwords . align
  where
    align :: [String] -> [String]
    align (x1 : xs) = (fillLeft 7 x1 ++ ": ") : xs
    align xs = xs

-- ----------------------------------------
--
-- trace an instruction during prog execution

instrTrc :: (OpCode -> Maybe Mnemonic) -> (String -> MicroInstr v) -> MInstr -> Offset -> MicroInstr v
instrTrc mns cmd ins pc' =
  cmd $ prettyInstr (indent pc') (prettyOp mns) (prettyJmp pc') prettyLab ins

-- ----------------------------------------

indent :: Offset -> String -> String
indent pc' xs =
  fillLeft 7 (show pc') ++ ":  " ++ xs

prettyOp :: (OpCode -> Maybe Mnemonic) -> Int -> String
prettyOp mns op' =
  fromMaybe ("undefinded-opcode: " ++ show op') $ mns op'

prettyJmp :: Offset -> Int -> [String]
prettyJmp pc' disp =
  [show disp, "--> " ++ show (fromIntegral pc' + disp)]

prettyLab :: Int -> String
prettyLab disp =
  show disp ++ ":"

-- ----------------------------------------
