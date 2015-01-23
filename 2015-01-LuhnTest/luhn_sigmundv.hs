-- Run as: runhaskell luhn.hs

import Data.Char (digitToInt)

-- First we convert the given integer to a list of integers

toDigits :: Int -> [Int]
toDigits n
	| length (show n) > 0 = digitToInt(head (show n)) : [digitToInt(i) | i <- tail (show n)]
	| otherwise = []

-- Then we define a helper function for reversing the given integer
	
toDigitsRev :: Int -> [Int]
toDigitsRev n = reverse (toDigits n)

-- Now it's time to define the function that doubles every second digit of the integer

doubleSecond :: [Int] -> [Int]
doubleSecond [] = []
doubleSecond [x] = [x]
doubleSecond (x0:x1:xs) = x0 : (2*x1) : doubleSecond xs

-- Before we combine everything into one function that checks if the credit card number is valid
-- we define a function for adding up all the digits of the integer

sumDigits :: [Int] -> Int
sumDigits xs = sum (concat [toDigits n | n <- xs])

-- Finally we combine all the functions above into one function
-- that checks the validity of a given credit card number

isValid :: Int -> Bool
isValid n = mod (sumDigits (doubleSecond (toDigitsRev n) ) ) 10 == 0

main :: IO()
main = do
	print (map isValid [49927398716,49927398717,1234567812345678,1234567812345670])
