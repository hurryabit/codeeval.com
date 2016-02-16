module Main where

import Data.Bits
import System.Environment

main = fmap head getArgs >>= readFile >>= mapM_ (print . process . read) . lines

process :: Int -> Int
process 0 = 0
process n = n .&. 1 + process (n `shiftR` 1)
