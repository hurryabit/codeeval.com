import System.Environment

main = do
  [file] <- getArgs
  contents <- readFile file
  let fibs :: [Integer]
      fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
  mapM_ (print . (fibs !!) . read) (lines contents)