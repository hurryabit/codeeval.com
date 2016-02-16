import Data.Array
import System.Environment

main = getArgs >>= readFile . head >>= mapM_ (putStrLn . unwords . process . words) . lines

process :: [String] -> [String]
process as =
    let n = round . sqrt . fromIntegral $ length as
        arr = listArray ((1,1),(n,n)) as
    in  [ arr ! (x,y) | y <- [1..n], x <- [n,n-1..1] ]