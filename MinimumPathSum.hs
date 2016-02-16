import Data.List
import System.Environment (getArgs)

type Matrix a = [[a]]

minPathSum :: Matrix Int -> Int
minPathSum = last . foldl combine (0:repeat maxBound)
  where
    combine ps xs =
      let qs = zipWith3 (\p q x -> min p q + x) ps (maxBound:qs) xs
      in  qs

split :: Eq a => a -> [a] -> [[a]]
split x = unfoldr uncombine . flip (,) True
  where
    uncombine ([],True ) = Nothing
    uncombine ([],False) = Just ([],([],True))
    uncombine (xs,_    ) =
      let (ys,zs) = break (x ==) xs
      in  Just (ys,(drop 1 zs,null zs))

parseInstances :: String -> [Matrix Int]
parseInstances = unfoldr uncombine . lines
  where
    uncombine []     = Nothing
    uncombine (l:ls) =
      let (ms,ls') = splitAt (read l) ls
      in  Just (map (map read . split ',') ms,ls')

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (print . minPathSum) (parseInstances input)
