primes = sieve [2..] 
  where sieve (p:xs) = p:sieve (filter ((0 /=) . (`mod` p)) xs)

isPalindrome xs = xs == reverse xs

main = print . head . filter (isPalindrome . show) . reverse . takeWhile (< 1000) $ primes