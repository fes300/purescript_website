module Chap4 where

import Prelude

import Control.MonadZero (guard)
import Data.Array (find, null, range)
import Data.Array.Partial (head, tail)
import Data.Foldable (find, foldl)
import Data.Int (even)
import Data.List (List(..), (..), (:))
import Data.Maybe (Maybe(..))
import Data.Path (Directory(..), File(..), Path(..), filename, root)
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

-- (Difficult!) Write a function factorizations which produces all factorizations of an integer n,
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

-- (Easy) Use foldl to test whether an array of boolean values are all true.
allTrue :: List Boolean -> Boolean
allTrue = foldl (\x -> \y -> x && y) true

-- (Medium) Characterize those arrays xs for which the function foldl (==) false xs returns true.
-- response = all `Array Boolean` with an add number of `false`

-- (Easy) Write a function onlyFiles which returns all files (not directories) in all subdirectories of a directory.
onlyFiles :: Path -> Array File
onlyFiles (DirectoryPath (Directory d paths)) = do
  path <- paths
  onlyFiles path
onlyFiles (FilePath f) = [f]

-- (Medium) Write a fold to determine the largest and smallest files in the filesystem.
maxFile :: Path -> Maybe File
maxFile = maxFile' <<< onlyFiles
  where
    maxFile' :: Array File -> Maybe File
    maxFile' [] = Nothing
    maxFile' a = Just (foldl (getBiggerFile) (unsafePartial head a) a)
    getBiggerFile :: File -> File -> File
    getBiggerFile (File name2 size2) (File name size) = if (size2 > size) then File name2 size2 else File name size

-- (Difficult) Write a function whereIs to search for a file by name. The function should return a value of type Maybe Path, indicating the directory containing the file, if it exists. It should behave as follows:
-- ex:
--  whereIs "/bin/ls"
--  Just (/bin/)


--  whereIs "/bin/cat"
--  Nothing
-- Hint: Try to write this function as an array comprehension using do notation

type FileName = String

whereIs :: FileName -> Maybe File
whereIs fileName = do
  path <- root
