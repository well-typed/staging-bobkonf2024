{-# LANGUAGE TemplateHaskell #-}
module Intro where

-- github repo with materials: https://github.com/well-typed/staging-bobkonf2024

-- Some other tutorials / talks:
--
-- Matthew Pickering
-- Beautiful Template Haskell
--
-- Andres Löh
-- Zero-overhead Abstractions in Haskell using Staging
--

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

pow5 :: Int -> Int
pow5 x = x * x * x * x * x

mul :: Int -> Int -> Int
mul x y = x * y

power :: forall m a. (Quote m, Num a) => Int -> Code m a -> Code m a
power 0 x = [|| 1 ||]
power 1 x = x
power n x = [|| $$(power (n - 1) x) * ($$x :: a) ||] -- power (n - 1) x * x

-- strange :: Quote m => Code m (a -> a)
strange x = [|| $$(liftTyped x) ||]

makeList :: Quote m => [Code m a] -> Code m [a]
makeList [] = [|| [] ||]
makeList (x : xs) = [|| $$x : $$(makeList xs) ||]

-- >>> let pow5' x = $$(power 5 [|| x ||]) in pow5' 2
-- 32

-- pow5' :: Int -> Int
-- pow5' x = $$(power 5 [|| x ||])

-- pow5' :: Int -> Int
-- pow5' = power 5

pred :: Char -> Bool
pred x = x `elem` "abcdefghij"

selem :: (Quote m, Eq a, Lift a) => Code m a -> [a] -> Code m Bool
selem _ []       = [|| False ||]
selem x (y : ys) = [|| $$x == $$(liftTyped y) || $$(selem x ys) ||]

