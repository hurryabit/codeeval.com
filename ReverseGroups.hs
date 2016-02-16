import qualified Data.ByteString.Char8 as BS
import Data.List (unfoldr)
import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    input <- BS.readFile inpFile
    mapM_ (BS.putStrLn . process) $ BS.lines input

process str = BS.intercalate (BS.pack ",") . concatMap (\xs -> if length xs < n then xs else reverse xs) . blocks n . BS.split ',' $ BS.init bef
    where (bef,aft) = BS.breakEnd (';' ==) str
          Just (n,_) = BS.readInt aft

blocks :: Int -> [a] -> [[a]]
blocks n = unfoldr split
    where split [] = Nothing
          split xs = Just $ splitAt n xs
