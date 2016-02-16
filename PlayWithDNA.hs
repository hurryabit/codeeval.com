import Data.Array
import Control.Monad
import Data.List
import System.Environment (getArgs)

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  forM_ (lines input) $ \line -> do
    let [p,n,s] = words line
        ts = matches p (read n) s
        out | null ts   = "No match"
            | otherwise = unwords ts
    putStrLn out

distance :: Eq a => [a] -> [a] -> Int
distance xs = last . foldl combine [0..] . zip [1..]
  where
    combine ds (i,y) =
      let es = i : zipWith4 f xs ds (tail ds) es
          f x d0 d1 e0
            | x == y    = d0
            | otherwise = 1 + minimum [d0,d1,e0]
      in  es

matches :: String -> Int -> String -> [String]
matches p n s = map snd $ sort $ do
  let lp = length p
      ls = length s
  t <- map (take lp) $ take (ls-lp+1) $ tails s
  let m = distance t p
  guard $ m <= n
  return (m,t)
