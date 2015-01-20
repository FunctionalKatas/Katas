module Main where

import Data.Char

odds :: Show x => [x] -> [x]
odds []     = []
odds (x:[]) = [x]
odds xs     = head xs : odds (drop 2 xs)

evens :: Show x => [x] -> [x]
evens [] = []
evens xs = odds . tail $ xs

sum_ciphers :: Int -> Int
sum_ciphers x
    | x < 10 = x
    | otherwise = (sum_ciphers (x `quot` 10)) + (x `mod` 10)

luhn_checksum :: String -> Bool
luhn_checksum code = let ciphers = map digitToInt $ reverse code
                         s1 = sum $ odds ciphers
                         s2 = sum $ map (sum_ciphers . (* 2)) $ evens ciphers
                         divisible a b = a `mod` b == 0
                     in (s1 + s2) `divisible` 10

show_luhn :: String -> IO ()
show_luhn code = putStrLn $ "Is " ++ (show code) ++ " valid? " ++ (show . luhn_checksum $ code)

main = do show_luhn "49927398716"
          show_luhn "49927398717"
          show_luhn "1234567812345678"
          show_luhn "1234567812345670"
