import Data.List
import System.Environment (getArgs)

main = getArgs >>= readFile . head >>= mapM_ (putStrLn . unwords . map show . concatMap (\g -> [length g,head g]) . group . map (read :: String -> Int) . words) . lines