import Data.List
import Data.Ord
import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    num:input <- fmap lines (readFile inpFile)
    mapM_ putStrLn . take (read num) $ sortBy (flip $ comparing length) input
