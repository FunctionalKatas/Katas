import System.Environment

odds :: [a] -> [a]
odds (x₁:x₂:xs) = x₁ : odds xs
odds xs = xs

evens :: [a] -> [a]
evens (x₁:x₂:xs) = x₂ : evens xs
evens _ = []

digit :: Char -> Int
digit x = read [x]

luhn :: String -> Bool
luhn cc = l == 0
  where
    ds = map digit $ reverse cc
    s₁ = sum $ odds ds
    s₂ = sum $ map (f . (*2)) $ evens ds
      where
        f x | x > 9 = sum $ map digit $ show x
            | otherwise = x
    l = digit $ last $ show $ s₁ + s₂

main :: IO ()
main = do
    [cc] <- getArgs
    print $ luhn cc
