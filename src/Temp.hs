
module Temp where

  import Data.List





  score' :: Code -> Code -> Score
  score' code guess = (c,w-c)
    where
      c = filterL (uncurry (==)) (zip guess code)
      w = filterL (`elem` guess) code

  filterL :: (a -> Bool) -> [a] -> Int
  filterL p xs = length $ filter p xs



  countWhite :: Code -> Code -> Int
  countWhite code guess = length $ filter (\x -> x `elem` guess) code

  compareLists:: Eq a => (a -> a -> Bool) -> [a] -> [a] -> Int
  countWhite p listA listB = filter (\x -> x `elem` listB) listA
