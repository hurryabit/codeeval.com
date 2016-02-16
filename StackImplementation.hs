module Main where

import System.Environment

main = fmap head getArgs >>= readFile >>= mapM_ (putStrLn . process) . lines

process :: String -> String
process = unwords . map show . (everyOther :: [Int] -> [Int]) . reverse . map read . words

everyOther :: [a] -> [a]
everyOther []         = []
everyOther [x1]       = [x1]
everyOther (x1:x2:xs) = x1:everyOther xs