import qualified Data.ByteString.Char8 as BS
import Data.Char
import System.Environment

main = do
	[inpFile] <- getArgs
	input <- BS.readFile inpFile
	mapM_ (\str -> BS.putStrLn (BS.concat [BS.take 40 str,BS.pack "***",BS.take 40 (BS.drop 40 str)]) >> BS.putStrLn (process str)) $ BS.lines input

process str
    | BS.length str <= 55 = str
    | otherwise           = (if BS.null short then BS.take 40 str else short) `BS.append` BS.pack "... <Read More>"
    where short = dropWhileEnd isSpace . dropWhileEnd (not . isSpace) . BS.take 40 $ str

dropWhileEnd p = fst . BS.spanEnd p