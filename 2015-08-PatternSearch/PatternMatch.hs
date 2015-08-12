module PatternMatch where

match :: String -> String -> Bool
match [] [] = True
match "*" [] = True
match "?" [] = True
match ('*':ps) (w:ws) = match ('*':ps) ws || match ps (w:ws) 
match ('?':ps) (w:ws) = match ps ws || match ps (w:ws)
match (p:ps) (w:ws) = p == w && match ps ws
match _ = False

matchInDict :: String -> [String] -> [String]
matchInDict pat = filter (match pat)

--- TESTS ---

testmatch :: IO ()
testmatch  = do
  mapM_ print $ [
    match "cr*sis" "crisis", -- True
    match "cr?si*" "crisis", -- True
    match "c*i" "cro", -- False
    match "*pqrs" "pqrst", -- False
    match "abc*bcd" "abcdhghgbcd", -- True
    match "abc*c?d" "abcd", -- False
    match "*c*d" "abcd", -- True
    match "*?c*d" "abcd", -- True
    match "abc**d" "abcd", -- True
    match "abc**d?" "abcde", -- True
    match "abc**d" "abcd", -- True
    match "ab?c" "abc", --True
    match "ab?c" "abcc", --True
    match "ab?c" "abc" --True
    ]

dict :: [String]
dict = ["haskell", "dshaskjb", "asdhasasd"]

test :: IO ()
test = print $ matchInDict "*hask*" dict
