module Main where

import Data.Bits
import Data.List
import System.Environment

main = fmap head getArgs >>= readFile >>= mapM_ (putStrLn . process) . lines

process :: String -> String
process "0" = "0"
process s   = reverse . unfoldr f . read $ s
    where f :: Int -> Maybe (Char,Int)
          f 0 = Nothing
          f n = Just (if testBit n 0 then '1' else '0',n `shiftR` 1)
