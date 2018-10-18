module Lib
    ( someFunc, Tree, emptyTree, testTree, lca
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree a = Empty | Node a (Tree a) (Tree a)

emptyTree :: Tree Int
emptyTree = Empty

testTree :: Tree Int
testTree = Node 1
  (Node 2
    (Node 4 Empty Empty)
    (Node 5
      (Node 8 Empty Empty)
      (Node 9 Empty Empty)))
  (Node 3
    (Node 6 Empty Empty)
    (Node 7 Empty Empty))

type Id = Int

data Path = [Id] :# !Int deriving (Eq, Show)

empty :: Path
empty = [] :# 0

cons :: Id -> Path -> Path
cons a (ys :# n) = (a:ys) :# (n + 1)

path1 :: Path
path1 = [1,2,4] :# 3

path2 :: Path
path2 = [1,2,5,9] :# 4

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where 
  k = min i j
  go n xxs@(x:xs) (y:ys) 
    | x == y   = xxs :# n
    | otherwise = go (n - 1) xs ys