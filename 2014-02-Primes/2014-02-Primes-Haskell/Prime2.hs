primes :: [Integer]
primes = 2:3:5:7:11:13:next
  where next = filter onlyPrimes $ scanl (+) 17 jumps
        onlyPrimes n = let l = (ceiling . sqrt . fromInteger) n in not $ any (\x -> mod n x == 0) $ takeWhile (<l) primes

jumps :: [Integer]
jumps = cycle [2,4]

main :: IO ()
main = print $ take 5 $ drop 44 $ filter (> 100000) primes
-- main = print $ take 20 primes

