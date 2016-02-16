import Data.List
import System.Environment (getArgs)

countDistSubSeq :: Eq a => [a] -> [a] -> Int
countDistSubSeq xs ys = last (foldl combine (repeat 1) xs)
  where
    combine ks x =
      let ls = 0 : zipWith3 (\k l y -> if x == y then k+l else l) ks ls ys
      in  ls

countDistSubSeqExt :: String -> String
countDistSubSeqExt line =
  let (seq,',':sub) = break (','==) line
  in  show (countDistSubSeq sub seq)

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (putStrLn . countDistSubSeqExt) (lines input)
