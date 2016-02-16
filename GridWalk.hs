import Data.Array
import Data.Char

main = print $ solve 19

dsum :: Int -> Int
dsum = sum . map digitToInt . show

solve :: Int -> Int
solve n = let xns   = takeWhile (\x -> dsum x <= n) [0..]
              x0ns  = listArray (0,n) $ map (\s -> intervals $ filter (\x -> dsum x <= s) xns) [0..n]
              y0ns  = scanl1 base $ map (\x -> x0ns ! (n-dsum x)) xns
              quad1 = sum . map (\(l,r) -> r-l+1) . concat $ tail y0ns
          in  4*quad1+1

intervals :: [Int] -> [(Int,Int)]
intervals xs = fuse [ (x,x) | x <- xs ]
    where 
        fuse :: [(Int,Int)] -> [(Int,Int)]
        fuse ((l1,r1):(l2,r2):lrs)
            | succ r1 == l2 = fuse ((l1,r2):lrs)
            | otherwise     = (l1,r1):fuse ((l2,r2):lrs)
        fuse lrs            = lrs

base :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
base [] _       = []
base _  []      = []
base old@((lo,ro):old') new@((ln,rn):new')
    | ro < ln   =         base old' new
    | rn < lo   =         base old  new'
    | rn < ro   = (ln,rn):base old  new'
    | otherwise = (ln,rn):base old' new'

{-
import qualified Data.Set as Set

isAccessible :: Int -> (Int,Int) -> Bool
isAccessible n (x,y) = dsum (abs x) + dsum (abs y) <= n

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [ (f x,g y) | (f,g) <- [(succ,id),(pred,id),(id,succ),(id,pred)] ]

solve' :: Int -> Int
solve' n = Set.size . fst $ until (Set.null . snd) step (Set.empty,Set.singleton (0,0))
    where step (old,cur) = let old' = old `Set.union` cur
                               cur' = (Set.fromList . filter (isAccessible n) . concatMap neighbors $ Set.toList cur) `Set.difference` old'
                           in  (old',cur')

histo n = scanl1 (+) . elems . accumArray (+) 0 (0,n) $ map (flip (,) 1) . takeWhile (<= n) $ map dsum [0..]
-}