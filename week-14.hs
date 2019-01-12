main = do
    print tree1
    print $ bstInsert tree1 9
    print $ bstToList tree1
    print $ bstRemove tree1 5
    print $ bstRemove tree1 7
    print $ getAndRemoveMax tree1
    print $ bstRemove' tree1 5
    print $ bstRemove' tree1 7

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

tree1 :: Tree Int
tree1 = Node 5
            (Node 1 Empty Empty)
            (Node 7
                (Node 6 Empty Empty)
                (Node 10 Empty Empty))

bstInsert :: Ord a => Tree a -> a -> Tree a
bstInsert Empty x = Node x Empty Empty
bstInsert (Node v left right) x
    | v == x = Node v left right
    | v > x = Node v (bstInsert left x) right
    | v < x = Node v left (bstInsert right x)

bstToList :: Tree a -> [a]
bstToList Empty = []
bstToList (Node v left right) = bstToList left ++ [v] ++ bstToList right

bstRemove :: Ord a => Tree a -> a -> Tree a
bstRemove Empty _ = Empty
bstRemove (Node v left right) x
    | v > x = Node v (bstRemove left x) right
    | v < x = Node v left (bstRemove right x)
    | v == x = foldl bstInsert left (bstToList right)

getAndRemoveMax :: Ord a => Tree a -> (a, Tree a)
getAndRemoveMax (Node x left Empty) = (x, left)
getAndRemoveMax (Node x left right) = (max, Node x left right') where
    (max, right') = getAndRemoveMax right
    
bstRemove' :: Ord a => Tree a -> a -> Tree a
bstRemove' Empty _ = Empty
bstRemove' tree@(Node v left right) x
    | v > x = Node v (bstRemove left x) right
    | v < x = Node v left (bstRemove right x)
    | v == x = removeRoot tree where
        removeRoot (Node _ Empty right) = right
        removeRoot (Node _ left Empty) = left
        removeRoot (Node _ left right) = Node leftMax left' right where
            (leftMax, left') = getAndRemoveMax left