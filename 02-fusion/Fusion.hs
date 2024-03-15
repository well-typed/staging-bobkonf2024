{-# LANGUAGE TemplateHaskell #-}
module Fusion where

-- Further reading:
--
-- Oleg Kiselyov, Aggelos Biboudis, Nick Palladinos, Yannis Smaragdakis
-- Stream Fusion, to Completeness
--

pipelineExample :: Int -> Int
pipelineExample n = sum (filter odd (map (+7) (map (\ x -> x * x) [1 .. n])))

elemExample :: Char -> Bool
elemExample c = elem c ['a' .. 'i']

-- Plan:
--
-- Define a type BList that captures a list that is written as an application
-- of "build", i.e., abstracting over the types of the list constructors.

data BList a =
  ToDo

-- We should have a function runBList that gets us back to an original list:

runBList :: BList a -> [a]
runBList = undefined

-- We should now be able to reimplement all the functions we need on lists
-- in terms of BList, e.g.
--
-- elem :: Eq a => a -> BList a -> Bool
-- filter :: (a -> Bool) -> BList a -> BList a
-- ...
--
-- Finally, we should be able to stage BList and all functions operating on it.
