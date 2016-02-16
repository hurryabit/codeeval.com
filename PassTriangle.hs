import Data.List
import System.Environment

-- Dynamic programming can be so concise in Haskell. Wow!
main = getArgs >>= readFile . head >>= print . head . foldr1 (\lin acc -> zipWith3 (\l a1 a2 -> l + max a1 a2) lin acc (tail acc)) . map (map read . words) . lines
