module Chap4 where

import Prelude

import Control.MonadZero (guard)
import Data.Array (null, range)
import Data.Array.Partial (head, tail)
import Data.Foldable (find, foldl)
import Data.Int (even)
import Data.List (List(..), concat, filter, union, (..), (:))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- 2. (Medium) Write a recursive function which counts the number of even integers in an array.
countEven :: Array Int -> Int
countEven arr =
  if null arr
    then 0
    else countIfEven (unsafePartial head arr) + countEven (unsafePartial tail arr) where
      countIfEven :: Int -> Int
      countIfEven num = if even num then 1 else 0

-- 2. Write a function which uses do notation to find the cartesian product of two arrays,
-- i.e. the set of all pairs of elements a, b, where a is an element of the first array,
-- and b is an element of the second
cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
  el1 <- arr1
  el2 <- arr2
  pure [el1, el2]

-- A Pythagorean triple is an array of numbers [a, b, c] such that a² + b² = c².
-- Use the guard function in an array comprehension to write a function triples which takes
-- a number n and calculates all Pythagorean triples whose components are less than n.
-- Your function should have type Int -> Array (Array Int)
triples :: Int -> Array (Array Int)
triples n = do
  i <- range 1 n
  j <- range i n
  y <- range j n
  guard $ i * i + j * j == y * y
  pure [i, j, y]

-- Write a function factorizations which produces all factorizations of an integer n,
-- i.e. arrays of integers whose product is n. Hint: for an integer greater than 1,
-- break the problem down into two subproblems: finding the first factor, and finding the remaining factors
getTuple :: Int -> Int -> List Int
getTuple n m = (m : n / m : Nil)

getGenericFactorizations :: Int -> Int -> List (List Int)
getGenericFactorizations m n = foldl (recurse) Nil (getFirstDivider m n)
  where
    recurse :: List (List Int) -> Int -> List (List Int)
    recurse _ i = (i : n/i : Nil) : (getGenericFactorizations (i + 1) n)

getFirstDivider :: Int -> Int -> Maybe Int
getFirstDivider m n = if n > 2 && n > m then find (\x -> mod n x == 0) (m .. (n - 1))  else Nothing

getFactorizations :: Int -> List (List Int)
getFactorizations = getGenericFactorizations 2
