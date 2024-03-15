{-# LANGUAGE TemplateHaskell #-}
module Intro where

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

-- Exercise P1. Define a staged version of the power function.

power :: Int -> Int -> Int
power 0 _ = 1
power n x = power (n - 1) x * x

spower :: Quote m => Int -> Code m Int -> Code m Int
spower = undefined

-- Exercise P2. Define (in GHCi, or a separate module) an
-- instantiation of the staged power function to a concrete
-- integer 5, and observe the code being generated by
-- using the flag -ddump-splices.

-- >>> pow5 x = undefined

-- Exercise P3. Define a minimally modified version of
-- staged power:

spower' :: Quote m => Int -> Code m (Int -> Int)
spower' = undefined

-- Exercise P4. Define a version of power that exploits that
--
-- power (2 * n)     x = power n x * power n x
-- power (2 * n + 1) x = power n x * power n x * x
--
-- and then stage that version.

-- Exercise P5. Define functions apply and lambda.

apply :: Quote m => Code m (a -> b) -> Code m a -> Code m b
apply = undefined

lambda :: Quote m => (Code m a -> Code m b) -> Code m (a -> b)
lambda = undefined

-- Exercise P6. Define functions makeList and liftList.

makeList :: Quote m => [Code m a] -> Code m [a]
makeList = undefined

liftList :: (Quote m, Lift a) => [a] -> Code m [a]
liftList = undefined

-- Exercise P7. Define an unstaged and staged version of the
-- elem function.

elem :: Eq a => a -> [a] -> Bool
elem = undefined

selem :: (Quote m, Eq a) => Code m a -> [Code m a] -> Code m Bool
selem = undefined

-- Exercise P8. What could you potentially do better if the
-- type was
--
-- selem :: (Quote m, Eq a, Lift a) => Code m a -> [a] -> Code m Bool
--
-- What if you could restrict the type even more?

