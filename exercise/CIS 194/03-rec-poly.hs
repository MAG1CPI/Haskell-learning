{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1
each :: Int -> [a] -> [a]
each step lst
  | step > 0 =
      [lst !! x | x <- [step - 1, step * 2 - 1 .. (length lst - 1)]]
  | otherwise =
      lst

skips :: [a] -> [[a]]
skips [] = []
skips lst = [each i lst | i <- [1, 2 .. (length lst)]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x1 : res@(x2 : x3 : _)) =
  if x2 > x1 && x2 > x3
    then x2 : localMaxima res
    else localMaxima res
localMaxima _ = []

-- Exercise 3
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

countAll :: [Integer] -> [Integer] -> [Int]
countAll elemList lst = map (`count` lst) elemList

genLine :: Int -> [Int] -> String
genLine height = map (\x -> if x >= height then '*' else ' ')

genGraph :: [Int] -> String
genGraph cntLst = unlines $ (`map` [maxHeight, maxHeight - 1 .. 1]) (`genLine` cntLst)
  where
    maxHeight = maximum cntLst

histogram :: [Integer] -> String
histogram = (++ "==========\n0123456789\n") . genGraph . (`countAll` [0, 1 .. 9])
