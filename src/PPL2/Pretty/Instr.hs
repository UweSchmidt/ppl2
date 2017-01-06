module PPL2.Pretty.Instr where

import PPL2.Prim.Prelude
import PPL2.Prim.Instr

-- ----------------------------------------

prettyAddr :: Address -> String
prettyAddr (AbsA i) = "m[" ++ show i ++ "]"
prettyAddr (LocA i) = "l[" ++ show i ++ "]"

prettyInstr :: (String -> String) ->
               (OpCode -> String) ->
               (lab -> [String]) ->
               (lab -> String) ->
               Instr lab -> String
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
        Comp op    -> [prettyOp op]
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
