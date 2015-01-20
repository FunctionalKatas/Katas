
{- ## Simple recursive solution ##
Top down approach, reducing problems into subproblems
Not very efficient as it has to calculate the same 
subproblems many times
-}
count :: [Integer] -> Integer -> Integer
count _ 0 = 1
count [] _ = 0
count (c:cs) n | n >= c = count cs n + count (c:cs) (n-c)
               | otherwise = count cs n


-- 
{- ## Solution using lazy dynamic programming ##
Bottom up approach, start building a table with the
simplest problems, so that the results can be reused
in complex ones, notably improving efficiency, but 
incurring in more memory use.

    amount  0   1   2   3   4   5   6   7   8   9   ...
coins           ----------------------------------------
[]          1   0   0   0   0   0   0   0   0   0   ...
[1]         1   1   1   1   1   1   1   1   1   1   ...
[2,1]       1   1   2   2   3   3   4   4   5   5   ...
[5,2,1]     1   1   2   2   3   4   5   6   7   8   ...
...

Haskell allows for the lazy construction of the table,
in that way only the needed sub-problems are calculated on demand.
By being lazy, the table is potentially infinite in size, but it doesn't
incur any extra memory use. 

-}

countDP :: [Int] -> [Integer]
countDP = foldr addCoin (1:repeat 0)
    where addCoin c oldlist = newlist
            where newlist = (take c oldlist) ++ zipWith (+) newlist (drop c oldlist)


main = do
        print (count [100,50,25,10,5,1] 100)
        print (countDP [100,50,25,10,5,1] !! 100)
        {- The recursive solution doesn't finish in 
           a reasonable amount of time for big amounts -}
        --print (count [100,50,25,10,5,1] 100000)  
        print (countDP [100,50,25,10,5,1] !! 100000)
