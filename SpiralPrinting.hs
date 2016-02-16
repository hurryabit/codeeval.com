import Control.Monad
import Data.Array
import Data.Char
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (putStrLn . process) (lines input)

process str =
    let arr = parse str
    in  unwords $ spiral (snd $ bounds arr) id arr

parse :: String -> Array (Int,Int) String
parse = fst . head . readP_to_S p
    where p = do
            let int = readS_to_P reads
            n <- int
            char ';'
            m <- int
            char ';'
            css <- manyTill (skipSpaces >> munch (not . isSpace)) eof
            return $ listArray ((1,1),(n,m)) css

spiral :: (Int,Int) -> ((Int,Int) -> (Int,Int)) -> Array (Int,Int) String -> [String]
spiral (n,m) f arr
    | n <= 0 || m <= 0 = []
    | otherwise        = [ arr ! f (1,c) | c <- [1..m] ] ++ spiral (m,n-1) (\(r',c') -> f (c'+1,m+1-r')) arr