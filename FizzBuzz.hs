module Main where

import System.Environment

main = fmap head getArgs >>= readFile >>= mapM_ (putStrLn . process) . lines

process :: String -> String
process s = let
    [x,y,n] = map read (words s)
    f k | k `mod` x == 0 && k `mod` y == 0 = "FB"
        | k `mod` x == 0                   = "F"
        |                   k `mod` y == 0 = "B"
        | otherwise                        = show k
    in  unwords $ map f [1..n]
