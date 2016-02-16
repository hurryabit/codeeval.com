import Data.List
import System.Environment (getArgs)

possibilities :: Int -> String -> [String]
possibilities n = sequence . replicate n . map head . group . sort

possibilitiesExt :: String -> String
possibilitiesExt line =
  let (n,',':s) = break (',' ==) line
  in  intercalate "," (possibilities (read n) s)

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (putStrLn . possibilitiesExt) (lines input)
