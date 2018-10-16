module Lib
    ( someFunc, Tree, emptyTree, testTree, lca
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree a = Empty | Node a (Tree a) (Tree a)

type Path = [Int]

type Graph = [Path]

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

path1 :: Path
path1 = [1,2,4]

path2 :: Path
path2 = [1,2,5,9]

pathToTree :: Path -> Path -> Tree Int
pathToTree [] [] = Empty
pathToTree p1 p2 =
  let len1 = length p1
      len2 = length p2
  in case (len1 >= len2) of
    (True) -> pathToTreeImp (drop (len1 - len2) p1) p2
    (False) -> pathToTreeImp p1 (drop (len2 - len1) p2)

pathToTreeImp :: Path -> Path -> Tree Int
pathToTreeImp p1 p2 = (Node 1 Empty Empty)

lca :: Eq a => Tree a -> a -> a -> Either Bool a
lca Empty _ _ = Left False
lca (Node v tl tr) n1 n2 =
  let l = lca tl n1 n2
      r = lca tr n1 n2
      root = (v == n1) || (v == n2)
      sameNode = (n1 == n2)
  in case (l, r, root, sameNode) of
    (_          , _         , _     , True) -> Right n1
    (Right a    , _         , _     , _)    -> Right a
    (_          , Right a   , _     , _)    -> Right a
    (Left True  , Left True , _     , _)    -> Right v
    (Left True  , _         , True  , _)    -> Right v
    (_          , Left True , True  , _)    -> Right v
    (Left True  , _         , False , _)    -> Left True
    (_          , Left True , False , _)    -> Left True
    (_          , _         , True  , _)    -> Left True
    _ -> Left False