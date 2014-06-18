data BinTree a = Node (BinTree a) a (BinTree a)
               | Empty
               deriving Show

insert :: Ord a => a -> BinTree a -> BinTree a
insert v Empty = Node Empty v Empty
insert v (Node l a r) | v >= a = Node l a (insert v r)
                      | v <  a = Node (insert v l) a r

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f Empty = Empty
mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)

foldTree :: (a -> b -> a) -> a -> BinTree b -> a
foldTree f z Empty = z
foldTree f z (Node l a r) =  foldTree f leftfold r
    where leftfold = foldTree f z l `f` a

asList :: BinTree a -> [a]
asList t = foldTree (++) [] $ mapTree return t
