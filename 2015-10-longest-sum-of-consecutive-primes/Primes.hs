import Data.Numbers.Primes -- outsource the hard bit to someone else!

type Prime = Integer   
type SequenceLength = Integer

longestPrimeSumOfConsecutivePrimes :: Integer -> Prime
longestPrimeSumOfConsecutivePrimes primeBelow = 
    let upperBoundOfTerms = length $ takeWhile (<primeBelow) $ scanl (+) 0 primes 
    in  snd $ go primeBelow (take upperBoundOfTerms primes) (0,0)

go :: Integer -> [Prime] -> (SequenceLength,Prime) -> (SequenceLength,Prime)
go primeBelow candidatesPrimes previousBestSoFar 
    | primesLeftToCheck > fst bestSoFar = go primeBelow (tail candidatesPrimes) bestSoFar       
    | otherwise = bestSoFar
        where candidate = last $
                          filter (isPrime.snd) $
                          takeWhile ((<primeBelow).snd) $
                          zip [0..] $ 
                          scanl (+) 0 candidatesPrimes
              bestSoFar 
                | fst candidate > fst previousBestSoFar = candidate
                | otherwise = previousBestSoFar
              primesLeftToCheck = toInteger (length candidatesPrimes)-1  

test = (longestPrimeSumOfConsecutivePrimes 100 == 41) &&
       (longestPrimeSumOfConsecutivePrimes 1000 == 953)

answer = longestPrimeSumOfConsecutivePrimes 1000000

