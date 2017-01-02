module PPL2.Memory.Stack
       (Stack', get, push, pop, top, new)
where

-- ----------------------------------------

newtype Stack' a = Stack [a]

-- ----------------------------------------

get :: Stack' a -> Maybe (a, Stack' a)
get (Stack (x : xs)) = Just (x, Stack xs)
get _                = Nothing

push :: a -> Stack' a -> Stack' a
push x (Stack xs) = Stack $ x : xs

pop :: Stack' a -> Maybe (Stack' a)
pop = fmap snd . get

top :: Stack' a -> Maybe a
top = fmap fst . get

new :: Stack' a
new = Stack []

-- ----------------------------------------
