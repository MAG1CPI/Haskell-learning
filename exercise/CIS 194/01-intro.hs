{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = (x `mod` 10) : toDigits (x `div` 10)

-- Exercise 2
{- 1
doubleEvery :: [Integer] -> [Integer]
doubleEvery [] = []
doubleEvery [x] = [x]
doubleEvery (x1 : x2 : xs) = x1 : (x2*2) : doubleEvery xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ doubleEvery $ reverse x
-}
doubleEveryTwo :: [Integer] -> [Integer]
doubleEveryTwo [] = []
doubleEveryTwo [_] = error "length should be 2*n"
doubleEveryTwo (x1 : x2 : xs) = (x1 * 2) : x2 : doubleEveryTwo xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x =
  if even $ length x
    then doubleEveryTwo x
    else head x : (doubleEveryTwo . tail) x

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = foldr (\ x -> (+) (sum $ toDigitsRev x)) 0

-- Exercise 4
validate :: Integer -> Bool
validate = even . sumDigits . doubleEveryOther . toDigits

-- Exercise 5
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dst tmp1 =
  hanoi (n - 1) src tmp1 dst
    ++ [(src, dst)]
    ++ hanoi (n - 1) tmp1 dst src

-- Exercise 6
{-
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ _ = []
hanoi' 1 src dst _ _ = [(src, dst)]
hanoi' 2 src dst tmp1 _ = [(src, tmp1), (src, dst), (tmp1, dst)]
hanoi' 3 src dst tmp1 tmp2 = [(src, tmp1), (src, tmp2), (src, dst), (tmp2, dst), (tmp1, dst)]
hanoi' n src dst tmp1 tmp2 =
  (hanoi' (n - x) src tmp1 dst tmp2) ++ [(src, tmp2)] ++ (hanoi' (n - x) tmp1 dst tmp2 src)
  where
    x = 1
-}
