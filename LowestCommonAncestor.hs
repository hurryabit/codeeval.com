module Main where

import System.Environment

main = fmap head getArgs >>= readFile >>= mapM_ (putStrLn . process) . lines

process :: String -> String
process = show . uncurry (lca sampleTree) . (\[x,y] -> (x,y)) . map read . words

data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving Show

leaf :: a -> Tree a
leaf x = Node x Empty Empty

sampleTree :: Tree Int
sampleTree = Node 30 (Node 8 (leaf 3) (Node 20 (leaf 10) (leaf 29))) (leaf 52)

lca :: Ord a => Tree a -> a -> a -> a
lca Empty _ _                                  = error "Empty tree"
lca (Node z l r) x y
    | (x <= z && z <= y) || (x >= z && z >= y) = z
    | x < z && y < z                           = lca l x y
    | x > z && y > z                           = lca r x y