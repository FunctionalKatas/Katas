{-
Functional Kats 2014-12-02

Author: Alexander Bich

Treap implementation - a binary search tree (Cartesian tree), automagically balanced by using random priorities.
The good thing about it - it's much easier (IMO) to understand and remember than red-black or AVL trees.

Supports merging and splitting in logarithmic time.

Implemeted in a usual immutable style.

Operations:
* build - builds a tree from a list. Takes O(N) time.
* split - splits a tree into left and right trees with left one having a given number of nodes. Takes O(log N) time.
* merge - merges two trees into one. Takes O(log N) time.
-}

import Data.Word
import Data.List
import Data.Function
import System.Random

-- | Node of treap.
data Node p v = Node Int p v (Maybe (Node p v)) (Maybe (Node p v)) deriving Show

-- | Add random numbers to the list.
randomize :: RandomGen rnd => rnd -> [a] -> [(Word8, a)]
randomize rnd xs = zip (randoms rnd) xs

-- | Build a treap from list.
build :: Ord p => [(p, v)] -> Maybe (Node p v)
build [] = Nothing
build values = Just $ Node totalCount maxP maxV leftNode rightNode where
	indexedValues = zip values [(1 :: Int)..]
	((maxP, maxV), maxIndex) = maximumBy (compare `on` (fst . fst)) indexedValues
	(leftValues, root : rightValues) = break ((>= maxIndex) . snd) indexedValues
	left = map fst leftValues
	right = map fst rightValues
	leftNode = build left
	rightNode = build right
	totalCount = 1 + (nodeSize leftNode) + (nodeSize rightNode)

-- | Convenience method to get size of Maybe Node.
nodeSize :: Maybe (Node p v) -> Int
nodeSize (Just (Node count _ _ _ _)) = count
nodeSize Nothing = 0

-- | Print tree.
printNode :: (Show p, Show v) => Maybe (Node p v) -> IO ()
printNode node = print1 0 node where
	print1 level maybeNode = do
		sequence $ replicate level $ putStr "    "
		case maybeNode of
			Just (Node totalCount p v left right) -> do
				putStrLn $ show totalCount ++ " " ++ show p ++ " " ++ show v
				print1 (level + 1) left
				print1 (level + 1) right
			Nothing -> putStrLn "***"

-- | Print tree as a linear sequence.
printNodeSeq :: (Show p, Show v) => Maybe (Node p v) -> IO ()
printNodeSeq maybeNode = case maybeNode of
	Just (Node count p v left right) -> do
		printNodeSeq left
		putStrLn $ show count ++ " " ++ show p ++ " " ++ show v ++ " "
		printNodeSeq right
	Nothing -> return ()

-- | Split tree into two, given a number of nodes in the left resulting tree.
splitNode :: Ord p => Int -> Maybe (Node p v) -> (Maybe (Node p v), Maybe (Node p v))
splitNode 0 Nothing = (Nothing, Nothing)
splitNode n (Just (Node totalCount p v left right)) = let leftCount = nodeSize left in
	if n <= leftCount then
		let 
			(leftLeft, leftRight) = splitNode n left
			newLeftRoot = Just $ Node (totalCount - leftCount) p v Nothing right
		in
			(leftLeft, mergeNode leftRight newLeftRoot)
	else
		let
			(rightLeft, rightRight) = splitNode (n - leftCount - 1) right
			newRightRoot = Just $ Node (leftCount + 1) p v left Nothing
		in
			(mergeNode newRightRoot rightLeft, rightRight)

-- | Merge two trees into one.
mergeNode :: Ord p => Maybe (Node p v) -> Maybe (Node p v) -> Maybe (Node p v)
mergeNode Nothing right = right
mergeNode left Nothing = left
mergeNode a@(Just (Node aCount ap av aLeft aRight)) b@(Just (Node bCount bp bv bLeft bRight)) =
	if ap > bp then Just (Node (aCount + bCount) ap av aLeft (mergeNode aRight b))
	else Just (Node (aCount + bCount) bp bv (mergeNode a bLeft) bRight)

-- Testing.
main :: IO ()
main = do
	-- first array
	let a = ["A", "B", "C", "D", "E", "F"]
	let at = build $ randomize (mkStdGen 548309) a

	-- second array
	let b = ["G", "H", "I", "J"]
	let bt = build $ randomize (mkStdGen 43554) b

	-- print first tree
	printNodeSeq at
	putStrLn "-----"
	printNode at

	putStrLn " "

	-- print second tree
	printNodeSeq bt
	putStrLn "-----"
	printNode bt
	putStrLn " "

	-- merge trees
	let abt = mergeNode at bt
	printNodeSeq abt
	putStrLn "-----"
	printNode abt

	-- split trees
	let (lt, rt) = splitNode 7 abt
	printNodeSeq lt
	putStrLn "-----"
	printNode lt
	putStrLn  " "
	printNodeSeq rt
	putStrLn "-----"
	printNode rt
