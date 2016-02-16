import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (putStrLn . process . read) $ lines input

findBoth :: (a -> Bool) -> [a] -> Maybe (Int,a)
findBoth p = run 0
    where run k []      = Nothing
          run k (x:xs)
            | p x       = Just (k,x)
            | otherwise = run (k+1) xs

revAndAdd :: Integer -> Integer
revAndAdd n = n + read (reverse $ show n)

isPalindrome xs = xs == reverse xs

process n =
    let Just (k,res) = findBoth (isPalindrome . show) $ iterate revAndAdd n
    in  unwords [show k,show res]