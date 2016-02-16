import qualified Data.IntMap as IM
import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (print . lun . map read . words) $ lines input

lun :: [Int] -> Int
lun = maybe 0 (head . fst) . IM.minView . IM.filter ((1 ==) . length) . IM.fromListWith (++) . flip zip (map (:[]) [1..])