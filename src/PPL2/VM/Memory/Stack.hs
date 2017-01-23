module PPL2.VM.Memory.Stack
       (Stack, get, getN, push, pop, top, new)
where

import PPL2.Prelude  ()
import PPL2.VM.Types

-- ----------------------------------------

newtype Stack a = Stack [a]

-- ----------------------------------------

get :: Stack a -> Maybe (a, Stack a)
get (Stack (x : xs)) = Just (x, Stack xs)
get _                = Nothing

-- look into the stack, offset cells below top of stack
getN :: Offset -> Stack a -> Maybe a
getN i (Stack xs)
  | (v : _xs') <- drop (fromIntegral i) xs =
      Just v
  | otherwise =
      Nothing

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x : xs

pop :: Stack a -> Maybe (Stack a)
pop = fmap snd . get

top :: Stack a -> Maybe a
top = fmap fst . get

new :: Stack a
new = Stack []

-- ----------------------------------------
