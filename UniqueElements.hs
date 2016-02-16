import Data.List
import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (putStrLn . process) $ lines input

process :: String -> String
process str =
    let xs :: [Int]
        xs = read $ "[" ++ str ++ "]"
    in intercalate "," . map (show . head) $ group xs