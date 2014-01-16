import Data.List
import Data.List.Split
import System.Exit
import Test.HUnit

-- Calculate sum of numbers in string, separated by commas or newlines by
-- default. First line contains list of separators if it starts by "//".
-- Separators can have multiple characters if enclosed in brackets (example:
-- "[***]"). Ignores numbers larger than 1000. Returns a Left value with a list
-- of negative integers if there is any, indicating an error.
calc :: String -> Either [Int] Int
calc s = let ls   = filter (<=1000) . map read . splitList $ findDelim s
             negs = filter (<0) ls
         in case negs of [] -> Right $ sum ls
                         _  -> Left negs

-- Input is a tuple containing a list of separators, and a string to split
-- using the provided separators. Output is the split string.
splitList :: ([String], String) -> [String]
splitList (ds,ns) = go ds [ns]
    where sl = split . dropDelims . dropBlanks . onSublist
          go (r:rs) acc = go rs $ concatMap (sl r) acc
          go [] acc = acc

-- Take the first line of the intput string to be a list of separators if it
-- starts with "//". Output list of separators and rest of the string. If no
-- line starts with "//", default separators are comma and newline.
findDelim :: String -> ([String], String)
findDelim ('/':'/':xs) = go [] False xs
    where go :: [String] -> Bool -> String -> ([String], String)
          go acc multi (y:ys)
            | multi = case y of
                ']' -> go acc False ys
                _   -> go ((head acc ++ [y]) : tail acc) True ys
            | y == '\n' = (acc, ys)
            | y == '[' = go ([]:acc) True ys
            | otherwise = go ([y]:acc) False ys
          go acc _ [] = (acc, [])
findDelim xs = ([",","\n"],xs)

tests :: [Assertion]
tests =
    [ assertEqual "Empty string" (calc "") (Right 0)
    , assertEqual "Single number" (calc "1") (Right 1)
    , assertEqual "Lwo numbers" (calc "1,2") (Right 3)
    , assertEqual "Newline and comma" (calc "1\n2,3") (Right 6)
    , assertEqual "Larger than 1000" (calc "1,2,1001") (Right 3)
    , assertEqual "Exactly 1000" (calc "1,2,1000") (Right 1003)
    , assertEqual "Custom separators" (calc "//;\n1;2") (Right 3)
    , assertEqual "Negative" (calc "1,2,-3,4,-5") (Left [(-3),(-5)])
    , assertEqual "Complex separators"
                  (calc "//[***][---];.\n1;2.3***4---5") (Right 15)
    ]

main :: IO ()
main = do
    c <- runTestTT . TestList $ map TestCase tests
    if failures c > 0 then exitFailure else return ()
