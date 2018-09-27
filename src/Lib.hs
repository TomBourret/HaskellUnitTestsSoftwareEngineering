module Lib
    ( someFunc, Tree, emptyTree, lca
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree a = Empty | Node a (Tree a) (Tree a)

emptyTree :: Tree Int
emptyTree = Empty

lca :: Tree a -> a -> a -> Either Bool a
lca Empty _ _ = Left False