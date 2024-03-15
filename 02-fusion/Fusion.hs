{-# LANGUAGE TemplateHaskell #-}
module Fusion where

import Prelude hiding (all, and, any, elem, enumFromTo, filter, foldr, map, or, sum)
import qualified Prelude

-- Further reading:
--
-- Oleg Kiselyov, Aggelos Biboudis, Nick Palladinos, Yannis Smaragdakis
-- Stream Fusion, to Completeness
--

-- Original examples we'd like to be able to write:
--
-- pipelineExample :: Int -> Int
-- pipelineExample n = sum (filter odd (map (+7) (map (\ x -> x * x) [1 .. n])))
-- 
-- elemExample :: Char -> Bool
-- elemExample c = elem c ['a' .. 'i']

-- Plan:
--
-- Define a type BList that captures a list that is written as an application
-- of "build", i.e., abstracting over the types of the list constructors.
--
-- type of nil:   [a]
-- type of cons:  a -> [a] -> [a]
--
-- if we abstract over the list / result type, they become
--
-- type of nil:   r
-- type of cons:  a -> r -> r
--
-- This motivates the definition of build:
--
-- build :: (forall r. (a -> r -> r) -> r -> r) -> [a]
-- build builder = builder (:) []
--
-- With build, we can define a list such as [1,2,3] like this instead:
--
-- example = build $ \ cons nil -> cons 1 (cons 2 (cons 3 nil))
--
-- The idea of foldr-build fusion is that if we have a function
-- given as a foldr, then if this is applied to a list built using
-- build, we can simplify
--
--   foldr op e (build builder) = builder op e
--
-- i.e., we can use the arguments of foldr instead of the list constructors
-- and never build the intermediate list at all!
--
-- Now to make this more precise ...
--
-- We use the "builder" as the definition of our BList type:

data BList a =
  MkBList (forall r. (a -> r -> r) -> r -> r)

-- Our version of build just produces a BList:

build :: (forall r. (a -> r -> r) -> r -> r) -> BList a
build = MkBList

-- The point of our definition is that foldr becomes trivial and
-- incorporates foldr-build fusion:

foldr :: (a -> r -> r) -> r -> BList a -> r
foldr op e (MkBList builder) = builder op e

-- Going from a BList a back to a [a] is just a foldr filling
-- in the original constructors again:

runBList :: BList a -> [a]
runBList = foldr (:) []

-- Let's now reimplement various list functions:

-- and :: [Bool] -> Bool
and :: BList Bool -> Bool
and = foldr (&&) True

-- or :: [Bool] -> Bool
or :: BList Bool -> Bool
or = foldr (||) False

-- filter :: (a -> Bool) -> [a] -> [a]
filter :: (a -> Bool) -> BList a -> BList a
filter p list =
  build $ \ cons nil ->
  foldr (\ x r -> if p x then x `cons` r else r) nil list

-- map :: (a -> b) -> [a] -> [b]
map :: (a -> b) -> BList a -> BList b
map f list =
  build $ \ cons nil ->
  foldr (\ x r -> f x `cons` r) nil list

-- all :: (a -> Bool) -> [a] -> Bool
all :: (a -> Bool) -> BList a -> Bool
all p = and . map p

-- any :: (a -> Bool) -> [a] -> Bool
any :: (a -> Bool) -> BList a -> Bool
any p = or . map p

-- elem :: Eq a => a -> [a] -> Bool
elem :: Eq a => a -> BList a -> Bool
elem x = any (== x)

-- foldl' :: (r -> a -> r) -> r -> [a] -> r
-- foldl' as foldr is a little bit tricky ...
foldl' :: (r -> a -> r) -> r -> BList a -> r
foldl' op e list =
  foldr (\ x r !acc -> r (op acc x)) id list e

-- sum :: Num a => [a] -> a
sum :: Num a => BList a -> a
sum = foldl' (+) 0

-- product :: Num a => [a] -> a
product :: Num a => BList a -> a
product = foldl' (*) 1

-- length :: [a] -> Int
length :: BList a -> Int
length = foldl' (\ r _ -> 1 + r) 0

-- listToMaybe :: [a] -> Maybe a
listToMaybe :: BList a -> Maybe a
listToMaybe = foldr (\ x _ -> Just x) Nothing

-- find :: (a -> Bool) -> [a] -> Maybe a
find :: Eq a => (a -> Bool) -> BList a -> Maybe a
find p = listToMaybe . filter p

-- enumFromTo :: Enum a => a -> a -> [a]
enumFromTo :: (Enum a, Ord a) => a -> a -> BList a
enumFromTo lo hi =
  build $ \ cons nil ->
    let
      loop !i
        | i > hi    = nil
        | otherwise = i `cons` loop (succ i)
    in
      loop lo

toBList :: [a] -> BList a
toBList list =
  build $ \ cons nil ->
    Prelude.foldr cons nil list

-- Our original examples are unchanged, except that we have to be explicit about enumFromTo:
--
pipelineExample :: Int -> Int
pipelineExample n = sum (filter odd (map (+7) (map (\ x -> x * x) (enumFromTo 1 n))))

elemExample :: Char -> Bool
elemExample c = elem c (enumFromTo 'a' 'i')

-- And they work:
--
-- >>> pipelineExample 7
-- 77
-- >>> elemExample 'x'
-- False
-- >>> elemExample 'b'
-- True
--
-- Even without staging, BLists behave really well! The reason is that
-- everything is non-recursive now, and therefore the inliner is much
-- more willing to perform the reductions we want.

-- Nevertheless, we can stage in order to get guaranteed fusion. That
-- version is in FusionStaged.hs.
