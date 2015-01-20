import Data.Char
import Data.List
import Data.Tuple
import qualified Data.Map.Strict as Map

-- | Add 'word' to the statistics map.
addStat :: Ord a => a -> Map.Map a Int -> Map.Map a Int
addStat a m = Map.insert a (1 + Map.findWithDefault 0 a m) m

-- | Count statistics for elements of the map.
calcStats :: Ord a => [a] -> [(Int, a)]
calcStats xs = maxStats where
	stats = Map.toList $ foldr addStat Map.empty xs
	maxStats = sortBy (flip compare) $ map swap stats

-- | Predicate "include the char into word?"
isWordChar :: Char -> Bool
isWordChar c = (isAlphaNum c) || c == '\''

-- | Unify word chars with toLower, convert all the rest to whitespace.
filterChar :: Char -> Char
filterChar c = if isWordChar c then toLower c else ' '

-- | Split text into words.
findWords :: String -> [String]
findWords = words . map filterChar

-- | Make list of consecutive pairs from a sequence.
makePairs :: [a] -> [(a, a)]
makePairs (x:y:z) = (x, y):(makePairs (y:z))
makePairs _ = []

main :: IO ()
main = do
	text <- readFile "book.txt"
	-- get a text as a sequence of words
	let textAsWords = findWords text

	-- calculate stats for words
	let wordStats = calcStats textAsWords
	putStrLn $ show $ take 50 wordStats

	-- calculate stats for consecutive pairs
	let pairStats = calcStats $ makePairs textAsWords
	putStrLn $ show $ take 50 pairStats
