
import Data.Array.ST  (newArray, readArray, writeArray, runSTUArray, STUArray)
import Data.Array.Unboxed  (UArray, assocs)
import Control.Monad
import Control.Monad.ST (ST)

sieve :: Int -> UArray Int Bool
sieve top = runSTUArray $ do
  ar <- fullsieve top
  mapM_ (sieveOut ar top) [2..t]
  return ar
  where t = floor $ sqrt (fromIntegral top)

fullsieve :: Int -> ST s (STUArray s Int Bool)
fullsieve top = newArray (1,top) True

-- 'strike out' multiples of i in the sieve array
sieveOut  :: STUArray s Int Bool -> Int -> Int -> ST s (STUArray s Int Bool)
sieveOut ar top i  = do
  isPrime <- readArray ar i
  if isPrime then
    mapM_ (strikeout ar) [ f x | x <- multipliers 0  ]
    else return ()
  return ar
  where i2 = i * i
        f x = i2 + x*i
        multipliers x | f x <= top = x : multipliers (x+1)
                      | otherwise = []

strikeout :: STUArray s Int Bool -> Int -> ST s ()
strikeout ar num = writeArray ar num False

primes :: Int -> [Int]
primes top = map fst $ filter (\(x,y) -> y) (assocs $ sieve top)

bigPrimes :: [Int]
bigPrimes = filter (> 100000) $ primes 200000

main :: IO ()
main = print $ take 5 $ drop 43 bigPrimes



