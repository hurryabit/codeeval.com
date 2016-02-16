import Data.List (init)
import Data.Ratio
import Data.Set as S
import System.Environment

main = getArgs >>= readFile . head >>= mapM_ (print . uncurry solve . parse) . lines

-- number of streets + number of avenues - number of junctions heli flies over
solve :: [Int] -> [Int] -> Int
solve (0:xl) (0:yl) =
    let asRatSet :: [Int] -> Set (Ratio Int)
        asRatSet = mapMonotonic fromIntegral . fromDistinctAscList
        xs = asRatSet xl
        ys = asRatSet yl
        q  = findMax ys / findMax xs
    in  size xs + size ys - size (ys `intersection` mapMonotonic (q *) xs)

parse :: String -> ([Int],[Int])
parse str =
    let (streets,' ':avenues) = break (' ' ==) str
        p s = read $ "[" ++ init (tail s) ++ "]"
    in  (p streets,p avenues)