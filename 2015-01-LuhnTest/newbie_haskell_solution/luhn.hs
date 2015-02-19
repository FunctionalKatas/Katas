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

import Data.Char
import Data.List

data Validity = Invalid | Valid deriving Eq
type CardNumber = String

luhnCheck :: CardNumber -> Validity
luhnCheck s = 
    let rs = map digitToInt . reverse $ s
        (os,es) = oddsAndEvens rs
        s1 = sum os
        s2 = sum . map (sumDigits . (*2)) $ es 
    in if (s1 + s2) `mod` 10 == 0 then Valid
       else Invalid

oddsAndEvens :: [Int] -> ([Int],[Int])
oddsAndEvens ds = 
    let dsWithFlag = zip ds (cycle [True,False])
        (osi, esi) = partition (\d -> snd d) dsWithFlag
    in (map fst osi,map fst esi)     

sumDigits:: Int -> Int
sumDigits = sum . map digitToInt . show

test :: Bool
test = let testData = ["49927398716", "49927398717", "1234567812345678","1234567812345670"]
    in  (map luhnCheck testData) == [Valid,Invalid,Invalid,Valid]
