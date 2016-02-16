import Data.List
import System.Environment

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (putStrLn . unwords . detect . words) $ lines input

detect :: Eq a => [a] -> [a]
detect xs = reverse . fst . head . filter (uncurry isPrefixOf) . tail $ zip (inits ys) (tails ys) 
    where ys = reverse xs
