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

lca :: Tree a -> a -> a -> Either Bool a
lca Empty _ _ = Left False