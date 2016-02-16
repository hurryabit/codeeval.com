module Main where

import Data.List
import System.Environment

main = fmap head getArgs >>= readFile >>= mapM_ (putStrLn . process . read) . lines

process n = intercalate " " $ map show $ concat $ take n triangle

triangle :: [[Int]]
triangle = map (takeWhile (>0)) $ iterate (\row -> zipWith (+) row (0:row)) (1:repeat 0)
