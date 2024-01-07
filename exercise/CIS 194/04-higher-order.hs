{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

{-
rotate :: (Ord a) => Tree a -> Tree a
rotate
  mid@( Node
          midHight
          leftChild
          midVal
          rightChild
        )
    | leftHeight - rightHeight >= 2 =
        let (Node _ ll leftVal lr) = leftChild
         in Node (midHight - 1) ll leftVal (Node (midHight - 2) lr midVal rightChild)
    | leftHeight - rightHeight <= -2 =
        let (Node _ rl rightVal rr) = rightChild
         in Node (midHight - 1) (Node (midHight - 2) leftChild midVal rl) rightVal rr
    | otherwise = mid
    where
      leftHeight = getNodeHeight leftChild
      rightHeight = getNodeHeight rightChild
rotate mid = mid
-}

rotateToRight :: (Ord a) => Tree a -> Tree a
rotateToRight (Node midHight leftChild@Node {} midVal rightChild) =
  Node (midHight - 1) ll leftVal (Node (midHight - 2) lr midVal rightChild)
  where
    (Node _ ll leftVal lr) = leftChild
rotateToRight mid = mid

rotateToLeft :: (Ord a) => Tree a -> Tree a
rotateToLeft (Node midHight leftChild midVal rightChild@Node {}) =
  Node (midHight - 1) (Node (midHight - 2) leftChild midVal rl) rightVal rr
  where
    (Node _ rl rightVal rr) = rightChild
rotateToLeft mid = mid

rotate :: (Ord a) => Tree a -> Tree a
rotate node@(Node _ leftChild _ rightChild)
  | leftHeight - rightHeight >= 2 = rotateToRight node
  | leftHeight - rightHeight <= -2 = rotateToLeft node
  | otherwise = node
  where
    leftHeight = getNodeHeight leftChild
    rightHeight = getNodeHeight rightChild
rotate Leaf = Leaf

getNodeHeight :: Tree a -> Integer
getNodeHeight Leaf = -1
getNodeHeight (Node nodeHeight _ _ _) = nodeHeight

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node 0 Leaf x Leaf
insert (Node _ leftChild nodeVal rightChild) x
  | x < nodeVal =
      let newLeftChild = insert leftChild x
       in rotate (Node (1 + max (getNodeHeight newLeftChild) (getNodeHeight rightChild)) newLeftChild nodeVal rightChild)
  | otherwise =
      let newRightChild = insert rightChild x
       in rotate (Node (1 + max (getNodeHeight leftChild) (getNodeHeight newRightChild)) leftChild nodeVal newRightChild)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr (flip insert) Leaf

-- Exercise 3
xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

{-
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
-}

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

listSubtract :: (Eq a) => [a] -> [a] -> [a]
listSubtract lst1 lst2 = [x | x <- lst1, x `notElem` lst2]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) ([1 .. n] `listSubtract` map (\(i, j) -> i + j + i * j * 2) (cartProd [1 .. n] [1 .. n]))
