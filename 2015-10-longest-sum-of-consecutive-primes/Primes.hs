import Data.List
import Data.Ord
import Data.Numbers.Primes -- outsource the hard bit to someone else!

longestPrimeSumOfConsecutivePrimes :: Integer -> (Integer,Integer)
longestPrimeSumOfConsecutivePrimes maxPrime = 
    let primesBelowLimit = takeWhile (<maxPrime) primes
        upperBoundOfTerms = length $ takeWhile (<maxPrime) $ scanl (+) 0 primesBelowLimit
    in  go maxPrime primesBelowLimit (take upperBoundOfTerms primesBelowLimit) (0,0)

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