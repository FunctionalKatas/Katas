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
    s₂ = sum $ map f $ evens ds
      where
        f x | x * 2 > 9 = sum $ map digit $ show $ x * 2
            | otherwise = x * 2
    l = digit $ last $ show $ s₁ + s₂

main :: IO ()
main = do
    [cc] <- getArgs
    print $ luhn cc
