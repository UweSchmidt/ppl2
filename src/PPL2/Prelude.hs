{-# LANGUAGE RankNTypes #-}

-- | the prelude for the whole system: generally useful stuff
--
-- all the included and reexported modules and
-- the functions declared in this module are
-- used within the whole system

module PPL2.Prelude
       ( module PPL2.Prelude
       , module Control.Lens
       , module Control.Monad
       , module Data.Maybe
       , module Data.Word
       )
where

import Control.Lens
import Control.Monad

import Data.Bifunctor
import Data.Maybe     hiding (fromJust)
import Data.Word      (Word)

-- ----------------------------------------
--
-- construct pais with an infix op to avoid (,)

infixr 1 .->

(.->) :: a -> b -> (a, b)
x .-> y = (x, y)

-- ----------------------------------------

-- a predicate as prism
--
-- I'm sure, Edward has already something like this
-- but I didn't find it in the lens package

prismPred :: (a -> Bool) -> Prism' a a
prismPred p = prism id
  (\ x -> case p x of
      True -> Right x
      _    -> Left  x
  )

-- ----------------------------------------
--
-- collect all element, which occur
-- more than once in a list
-- the result doesn't contain duplicates

dup :: Eq a => [a] -> [a]
dup = reverse . dup' []
  where
    dup' acc [] = acc
    dup' acc (x : xs)
      | x `elem` xs
        &&
        x `notElem` acc = dup' (x : acc) xs
      | otherwise       = dup'      acc  xs


-- ----------------------------------------
