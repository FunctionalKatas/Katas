primes = sieve odds

odds = [3,5..]

sieve (n:ns) = n : sieve [ x | x <- ns , x `mod` n /= 0 ]

bigPrimes = filter (> 100000) primes

main = print $ take 5 $ drop 43 bigPrimes

