{-
Given Credit Card number: 49927398716
• Reverse the digits: 61789372994
• Sum the odd digits: 6 + 7 + 9 + 7 + 9 + 4 = 42 = s1
• The even digits: 1, 8, 3, 2, 9
•  Two times each even digit: 2, 16, 6, 4, 18
•  Sum the digits of each multiplication: 2, 7, 6, 4, 9
•  Sum the last: 2 + 7 + 6 + 4 + 9 = 28 = s2
• s1 + s2 = 70
• which, as it ends in zero, means that 49927398716 passes the Luhn test
-}
import Data.Vect

data Validity = Valid | Invalid
instance Eq Validity where
  Valid == Valid = True
  Invalid == Invalid = True
  _ == _ = False

CardNumber : Type
CardNumber = Vect 11 Char

toDigit : Char -> Int
toDigit c = ord c - ord '0'

zwi : CardNumber -> List (Int,Char)
zwi cs = zip [1..11] $ toList cs

oddDigits : CardNumber -> List Int
oddDigits xs = map (toDigit . snd) $ filter (\c => ((fst c) `mod` 2) /= 0 ) (zwi xs)

calc_s1 : CardNumber -> Int
calc_s1  = sum . oddDigits

evenDigits : CardNumber -> List Int
evenDigits xs = map (toDigit . snd) $ filter (\c => ((fst c) `mod` 2) == 0 ) (zwi xs)

sumDigits : Int -> Int
sumDigits = sum . map toDigit . unpack . show

sumAllDigits : List Int -> List Int
sumAllDigits = map sumDigits

calc_s2 : CardNumber -> Int
calc_s2 = sum . sumAllDigits . map (*2) . evenDigits

luhnCheck : CardNumber -> Validity
luhnCheck x = if (((s1 + s2) `modInt` 10) == 0) then Valid else Invalid
  where r:CardNumber  = reverse x
        s1 = calc_s1 x
        s2 = calc_s2 x

test : Validity
test = luhnCheck $ fromList $ unpack "49927398716"

test2 : Validity
test2 = luhnCheck $ fromList $ unpack "49927398715"
