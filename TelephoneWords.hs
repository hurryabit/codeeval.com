import Data.Array
import Data.List
import System.Environment (getArgs)

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  let table = listArray ('0','9') ["0", "1", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"]
  mapM_ (putStrLn . intercalate "," . sequence . map (table !)) (lines input)
