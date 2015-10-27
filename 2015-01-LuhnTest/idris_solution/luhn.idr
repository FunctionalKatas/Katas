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
import Debug.Trace

data Validity = Valid | Invalid
instance Eq Validity where
  Valid == Valid = True
  Invalid == Invalid = True
  _ == _ = False

AllowableCardDigit : Type
AllowableCardDigit = Fin 10

CardNumber : Nat -> Type
CardNumber n = Vect n AllowableCardDigit

toDigit : Char -> Int
toDigit c = ord c - ord '0' -- Can't be the safest way to do this!

toAllowableCardDigit : Char -> AllowableCardDigit
toAllowableCardDigit c = the AllowableCardDigit (fromNat (toNat $ toDigit c))

zipWithIndex : Vect n t -> Vect n (Nat,t)
zipWithIndex cs = zip (map finToNat range) cs

pred : (Nat, t) -> Bool
pred (a,b) = ((toIntNat a) `mod` 2) == 0

odds : CardNumber n -> (p : Nat ** Vect p (Nat,AllowableCardDigit))
odds cn =  filter pred (zipWithIndex cn)

evens : CardNumber n -> (p : Nat ** Vect p (Nat,AllowableCardDigit))
evens cn =  filter (not . pred) (zipWithIndex cn)

calc_s1 : (CardNumber n) -> Nat
calc_s1 cn = sum $ map (finToNat . snd) $  getProof $ odds cn

sumDigits : Int -> Int
sumDigits = sum . map toDigit . unpack . show

sumAllDigits : List Int -> List Int
sumAllDigits = map sumDigits

calc_s2 : (CardNumber n) -> Int
calc_s2 cn = sum $ sumAllDigits . toList $ map ((*2) . toIntNat . finToNat . snd)  $ getProof $ evens cn

luhnCheck : (CardNumber n) -> Validity
luhnCheck {n} x = if (((s1 + s2) `modInt` 10) == 0) then Valid else Invalid
  where s1 = toIntNat $ calc_s1 $ reverse x
        s2 = calc_s2 $ reverse x

aCard : CardNumber 11
aCard = fromList $ map toAllowableCardDigit $ unpack "49927398716"

test : Validity
test = luhnCheck $ fromList $ map toAllowableCardDigit $ unpack "49927398716"

test2 : Validity
test2 = luhnCheck $ fromList $ map toAllowableCardDigit $ unpack "49927398715"
