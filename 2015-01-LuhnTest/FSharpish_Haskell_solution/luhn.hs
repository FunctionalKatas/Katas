module Luhn where

import Data.Char(digitToInt)

-- Define the F# pipe operator which is more intuitive than
-- the composition operator (.) for people not familiar with haskell
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

mapEverySecond :: (a -> a) -> [a] -> [a] 
mapEverySecond func listOfArgs = let functionList = cycle [id, func]
                                 in zipWith (|>) listOfArgs functionList

sumIf2Digits :: Int -> Int
sumIf2Digits n = let (tens, units) = n `divMod` 10
                 in tens + units

isMultipleOf :: Int -> Int -> Bool
isMultipleOf m n = n `mod` m  == 0

luhn :: String -> Bool
luhn c = c
         |> reverse
         |> map digitToInt
         |> mapEverySecond (*2)
         |> mapEverySecond sumIf2Digits
         |> sum
         |> isMultipleOf 10

main :: IO ()
main = do
       print $ map luhn ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
