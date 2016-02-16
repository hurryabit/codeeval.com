import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (print . count) $ lines input

primes = sieve [2..] 
  where sieve (p:xs) = p:sieve (filter ((0 /=) . (`mod` p)) xs)

count str =
    let (n,',':m) = break (',' ==) str
    in  length . takeWhile (<= read m) . dropWhile (< read n) $ primes