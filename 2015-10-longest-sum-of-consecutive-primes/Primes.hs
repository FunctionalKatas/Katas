import Data.List
import Data.Ord

primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
 where 
  sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
                  where (h,~(_:t)) = span (< p*p) xs


longestPrimeSumOfConsecutivePrimes :: Integer -> (Integer,Integer)
longestPrimeSumOfConsecutivePrimes maxPrime = 
    let primesBelowLimit = takeWhile (<maxPrime) primes
        upperBoundOfTerms = length $ takeWhile (<maxPrime) $ scanl (+) 0 primesBelowLimit
    in  go maxPrime primesBelowLimit (take upperBoundOfTerms primesBelowLimit) (0,0)

isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
     divisible y = x `mod`y == 0
     notTooBig y = y*y <= x

go :: Integer -> [Integer] -> [Integer] -> (Integer,Integer) -> (Integer,Integer)
go maxPrime allPrimes considering bestSoFar = 
    let b = filter (\idxEl -> isPrime (snd idxEl) && snd idxEl<maxPrime) $ 
                zip [0..] $ 
                scanl (+) 0 considering
        best = if null b then bestSoFar else maximumBy (comparing fst) b
        newBestSoFar = if fst bestSoFar > fst best then bestSoFar else best        
    in if toInteger (length considering) > fst newBestSoFar then go maxPrime allPrimes (tail considering) newBestSoFar
       else newBestSoFar

test = (longestPrimeSumOfConsecutivePrimes 100 == (6,41)) &&
       (longestPrimeSumOfConsecutivePrimes 1000 == (21,953))