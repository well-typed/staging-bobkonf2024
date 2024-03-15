{-# LANGUAGE TemplateHaskell #-}
module FusionStaged where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
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

data BList m a =
  MkBList (forall r. (Code m a -> Code m r -> Code m r) -> Code m r -> Code m r)

-- Our version of build just produces a BList:

build :: (forall r. (Code m a -> Code m r -> Code m r) -> Code m r -> Code m r) -> BList m a
build = MkBList

-- The point of our definition is that foldr becomes trivial and
-- incorporates foldr-build fusion:

foldr :: (Code m a -> Code m r -> Code m r) -> Code m r -> BList m a -> Code m r
foldr op e (MkBList builder) = builder op e

-- Going from a BList a back to a [a] is just a foldr filling
-- in the original constructors again:

runBList :: Quote m => BList m a -> Code m [a]
runBList = foldr (\ x r -> [|| $$x : $$r ||]) [|| [] ||]

-- Let's now reimplement various list functions:

-- and :: [Bool] -> Bool
and :: Quote m => BList m Bool -> Code m Bool
and = foldr (\ x r -> [|| $$x && $$r ||]) [|| True ||]

-- or :: [Bool] -> Bool
or :: Quote m => BList m Bool -> Code m Bool
or = foldr (\ x r -> [|| $$x || $$r ||]) [|| False ||]

-- filter :: (a -> Bool) -> [a] -> [a]
-- There's a problem with filter! See below!
filter' :: Quote m => (Code m a -> Code m Bool) -> BList m a -> BList m a
filter' p list =
  build $ \ cons nil ->
  foldr (\ x r -> [|| if $$(p x) then $$(x `cons` r) else $$r ||]) nil list

-- map :: (a -> b) -> [a] -> [b]
-- NOTE: map does not mention any quotes or splices. Its definition
-- is just as before.
map :: Quote m => (Code m a -> Code m b) -> BList m a -> BList m b
map f list =
  build $ \ cons nil ->
  foldr (\ x r -> f x `cons` r) nil list

-- all :: (a -> Bool) -> [a] -> Bool
-- NOT: the definitions of all and any also do not change.
all :: Quote m => (Code m a -> Code m Bool) -> BList m a -> Code m Bool
all p = and . map p

-- any :: (a -> Bool) -> [a] -> Bool
any :: Quote m => (Code m a -> Code m Bool) -> BList m a -> Code m Bool
any p = or . map p

-- elem :: Eq a => a -> [a] -> Bool
elem :: (Quote m, Eq a) => Code m a -> BList m a -> Code m Bool
elem x = any (\ y -> [|| $$y == $$x ||])

-- foldl' :: (r -> a -> r) -> r -> [a] -> r
-- foldl' as foldr is a little bit tricky ...
-- the staged version is even more tricky (yet in principle straight-forward)
foldl' :: Quote m => (Code m r -> Code m a -> Code m r) -> Code m r -> BList m a -> Code m r
foldl' op e list =
  [|| $$(foldr (\ x r -> [|| \ !acc -> $$r $$(op [|| acc ||] x) ||]) [|| id ||] list) $$e ||]

-- sum :: Num a => [a] -> a
sum :: (Quote m, Num a) => BList m a -> Code m a
sum = foldl' (\ r x -> [|| $$r + $$x ||]) [|| 0 ||]

-- product :: Num a => [a] -> a
product :: (Quote m, Num a) => BList m a -> Code m a
product = foldl' (\ r x -> [|| $$r * $$x ||]) [|| 1 ||]

-- length :: [a] -> Int
length :: Quote m => BList m a -> Code m Int
length = foldl' (\ r _ -> [|| 1 + $$r ||]) [|| 0 ||]

-- listToMaybe :: [a] -> Maybe a
listToMaybe :: Quote m => BList m a -> Code m (Maybe a)
listToMaybe = foldr (\ x _ -> [|| Just $$x ||]) [|| Nothing ||]

-- find :: (a -> Bool) -> [a] -> Maybe a
find :: (Quote m, Eq a) => (Code m a -> Code m Bool) -> BList m a -> Code m (Maybe a)
find p = listToMaybe . filter' p

-- enumFromTo :: Enum a => a -> a -> [a]
enumFromTo :: (Quote m, Enum a, Ord a) => Code m a -> Code m a -> BList m a
enumFromTo lo hi =
  build $ \ cons nil ->
    [||
      let
        loop !i
          | i > $$hi  = $$nil
          | otherwise = $$(cons [|| i ||] [|| loop (succ i) ||])
      in
        loop $$lo
    ||]

mkBList :: Quote m => [Code m a] -> BList m a
mkBList list =
  build $ \ cons nil ->
    Prelude.foldr cons nil list

staticBList :: (Quote m, Lift a) => [a] -> BList m a
staticBList =
  mkBList . (liftTyped <$>)

pipelineExample :: Quote m => Code m Int -> Code m Int
pipelineExample n = sum (filter' (\ x -> [|| odd $$x ||]) (map (\ x -> [|| $$x + 7 ||]) (map (\ x -> [|| $$x * $$x ||]) (enumFromTo [|| 1 ||] n))))

elemExample :: Quote m => Code m Char -> Code m Bool
elemExample c = elem c (staticBList ['a' .. 'j'])

-- These work:
--
-- >>> pipelineExample' n = $$(pipelineExample [|| n ||])
-- >>> pipelineExample' 7
-- 77
--
-- >>> elemExample' c = $$(elemExample [|| c ||])
-- >>> (elemExample' 'c', elemExample' 'x')
-- (True,False)
--
-- However, pipelineExample' has a duplication of code:
--
--   pipelineExample [|| n_a3vV ||]
--   ======>
--     (let
--        loop_a3wx !i_a3wy
--          | (i_a3wy > n_a3vV) = id
--          | otherwise
--          = if odd ((i_a3wy * i_a3wy) + 7) then
--                \ !acc_a3wz
--                  -> loop_a3wx (succ i_a3wy) (acc_a3wz + ((i_a3wy * i_a3wy) + 7))
--            else
--                loop_a3wx (succ i_a3wy)
--      in loop_a3wx 1)
--       0
--
-- Note that the i * i occurs twice. This is because filter' duplicates.
-- We can fix this by inserting a let:
--
filter :: Quote m => (Code m a -> Code m Bool) -> BList m a -> BList m a
filter p list =
  build $ \ cons nil ->
  foldr (\ x r -> [|| let x' = $$x in if $$(p [|| x' ||]) then $$([|| x' ||] `cons` r) else $$r ||]) nil list
