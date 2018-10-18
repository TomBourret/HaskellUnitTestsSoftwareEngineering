module Lib
    ( someFunc, empty, Path, path1, path2, path3, pathRes1, pathRes2, path_length, path_contains, lca
    ) where

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