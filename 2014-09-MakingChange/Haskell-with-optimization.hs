{-# OPTIONS_GHC -O2 #-}

module Main where

splitCoins' :: [Int] -> Int -> Int
splitCoins'  _ 0 = 1
splitCoins' [] _ = 0
splitCoins' coins@(h:t) r | r < 0 = 0
                          | otherwise = (splitCoins' coins (r - h)) + (splitCoins' t r)

-- edge {{{
splitCoins :: [Int] -> Int -> Int
splitCoins _ 0 = 0
splitCoins coins r = splitCoins' coins r
-- }}}

coins = [50,25,10,5,1]
-- coins = [50,20,10,5,2,1]

main :: IO ()
main = do (putStrLn.show) $ splitCoins coins 1000

