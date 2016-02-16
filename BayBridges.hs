import Control.Applicative
import Data.Array
import Data.Char
import Data.Function
import Data.List (maximumBy)
import Data.Maybe
import System.Environment

main = do
    [file] <- getArgs
    input <- readFile file
    mapM_ print . maxIndepSubset . edges . parseInput $ input

type Point = (Double,Double)

type Line = (Point,Point)

intersect :: Line -> Line -> Bool
intersect ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4))
    | det == 0  = False
    | otherwise = online (d*e-b*f) && online (a*f-c*d)
    where a = x1-x2
          b = x4-x3
          c = y1-y2
          d = y4-y3
          e = x2-x4
          f = y2-y4
          det = a*d-b*c
          online q = (0 <= q) && (q <= det) || (0 >= q) && (q >= det)

parseLine :: String -> Line
parseLine line = let (pre,':':' ':post) = span isDigit line
                     ([x1,y1],[x2,y2]) = read post
                 in  ((x1,y1),(x2,y2))

type LineTable = Array Int Line

parseInput :: String -> LineTable
parseInput input = let ls = lines input
                   in  listArray (1,length ls) (map parseLine ls)

type EdgeTable = Array (Int,Int) Bool

edges :: LineTable -> EdgeTable
edges tab = let (l,h) = bounds tab
                bnds = ((l,l),(h,h))
                f (u,v) = intersect (tab ! u) (tab ! v)
            in  listArray bnds $ map f (range bnds)

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

isIndep :: EdgeTable -> [Int] -> Bool
isIndep tab = not . any (tab !) . pairs

subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

indepSubsets :: EdgeTable -> [[Int]]
indepSubsets tab = let ((l,_),(h,_)) = bounds tab
                   in  filter (isIndep tab) (subsets [l..h])

maxIndepSubset :: EdgeTable -> [Int]
maxIndepSubset = maximumBy (compare `on` length) . indepSubsets