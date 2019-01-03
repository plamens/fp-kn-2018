main = do
    print True'
    print square1
    print $ area (Circle 5)
    print $ area (Rectangle 5 7)
    print person1
    print list1
    print $ length' list1
    print tree1
    print $ size tree1
    print $ depth tree1
    print $ nodesOnLevel tree1 2
    print tree2
    print $ listSpecial tree11
    
data Bool' = True' | False'
    deriving Show

data Seasons = Summer | Autumn | Winter | Spring

data Point = Point Float Float
    deriving (Show)

data Shape = Circle Float | Rectangle Float Float
    deriving (Show)

square1 :: Shape
square1 = Rectangle 5 5

area :: Shape -> Float
area (Circle r) = pi * pi * r
area (Rectangle a b) = a * b

data Person = Person {name :: String, age :: Int, height :: Float}
    deriving (Show)

person1 = Person {name="Ivan", age=25, height=190}

data List a = EmptyList | Cons a (List a)
    deriving (Show)

list1 :: List Int
list1 = Cons 10 (Cons 5 EmptyList)

length' :: List a -> Int
length' EmptyList = 0
length' (Cons _ tail) = 1 + length' tail

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

tree1 :: Tree Int
tree1 = Node 5
            (Node 1 Empty Empty)
            (Node 2 Empty (Node 10 Empty Empty))

size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

nodesOnLevel :: Tree a -> Int -> [a]
nodesOnLevel Empty _ = []
nodesOnLevel (Node x left right) level =
    if level == 1
        then [x]
        else nodesOnLevel left (level-1) ++ nodesOnLevel right (level-1)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)

tree2 = treeMap (\x -> take x $ repeat 'a') tree1

{-
Задача 1. Дефинирайте функцията listSpecial tree, която приема двоично дърво tree
и връща списък от тези стойности на върховете на tree, които са равни на сбора на
стойностите на децата си.
 
Пример:
    tree11 = (Node 3
                (Node 1
                    (Node 1 Empty Empty)
                    Empty)
                (Node 2
                    (Node 4 Empty Empty)
                    (Node 7 Empty Empty)))
   
    listSpecial tree11 -> [3, 1]
-}

tree11 = (Node 3
            (Node 1
                (Node 1 Empty Empty)
                Empty)
            (Node 2
                (Node 4 Empty Empty)
                (Node 7 Empty Empty)))

listSpecial :: (Num a, Eq a) => Tree a -> [a]
listSpecial Empty = []
listSpecial node@(Node x left right) =
    let sumOfChildren = sum (nodesOnLevel node 2)
        restSpecial = (listSpecial left) ++ (listSpecial right)
    in if x == sumOfChildren
        then x : restSpecial
        else restSpecial