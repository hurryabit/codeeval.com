primes = sieve [2..] 
  where sieve (p:xs) = p:sieve (filter ((0 /=) . (`mod` p)) xs)

main = print $ sum (take 1000 primes)