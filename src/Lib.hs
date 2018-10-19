module Lib
    ( someFunc, empty, Path, path1, path2, path3, graph, pathRes1, pathRes2, path_length, path_contains, lca, lca_graph
    ) where

import GHC.Exts

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Id = Int

data Path = [Id] :# !Int deriving (Eq, Show)

empty :: Path
empty = [] :# 0

-- cons :: Id -> Path -> Path
-- cons a (ys :# n) = (a:ys) :# (n + 1)

path1 = [8,5,4,2,1] :# 5
path2 =   [9,6,3,1] :# 4
path3 =   [8,7,3,1] :# 4

graph = [path1, path2, path3]

pathRes1 = [1] :# 1
pathRes2 = [3,1] :# 2

path_length :: Path -> Int
path_length (ys :# n) = n

path_contains :: Path -> Int -> Bool
path_contains ([] :# 0) _ = False
path_contains ((x:xs) :# l) n = 
  if x == n 
    then True
    else path_contains (xs :# (l-1)) n

lca :: Path -> Path -> Path
lca (xs0 :# i) (ys0 :# j) = go k (drop (i-k) xs0) (drop (j-k) ys0) where 
  k = min i j
  go _ [] _ = empty
  go _ _ [] = empty
  go n xxs@(x:xs) (y:ys) 
    | x == y   = xxs :# n
    | otherwise = go (n - 1) xs ys

lca_graph :: [Path] -> Int -> Int -> Path
lca_graph [] _ _ = empty
lca_graph graph n1 n2 = last (sortWith path_length (concat (map (select_paths graph n1 n2) graph)))

select_paths :: [Path] -> Int -> Int -> Path -> [Path]
select_paths graph n1 n2 path = map (apply_LCA path n1 n2) graph

apply_LCA :: Path -> Int -> Int -> Path -> Path
apply_LCA p n1 n2 path = 
  if (p /= path && (((p `path_contains` n1) && (path `path_contains` n2)) || ((p `path_contains` n2) && (path `path_contains` n1))))
    then lca path p
    else empty