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

lca :: Eq a => Tree a -> a -> a -> Either Bool a
lca Empty _ _ = Left False
lca (Node v tl tr) n1 n2 =
  let l = lca tl n1 n2
      r = lca tr n1 n2
      root = (v == n1) || (v == n2)
  in case (l, r, root) of
    (Left True, Left True, _) -> Right v
    (Left True, _, True) -> Right v
    (_, Left True, True) -> Right v
    (Left True, _, False) -> Left True
    (_, Left True, False) -> Left True
    (_, _, True) -> Left True
    _ -> Left False