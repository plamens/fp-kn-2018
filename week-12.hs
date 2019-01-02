main = do
    print $ reverse' [1, 2, 3]
    print $ compose [(+1), (2*)] 7
    print $ combinations [[1, 2, 3], [4], [5, 6]]

{-
foldl (+) 0 [1,2,3]
(((0 + 1) + 2) + 3)

foldr (+) 0 [1,2,3]
(1 + (2 + (3 + 0)))
-}

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

{-
Задача 1. Използвайте fold(l/r), за да дефинирате следните функции:
а). sum' xs, която връща сбора на елементите на xs.
б). product' xs, която връща произведението на елементите на xs.
в). length' xs, която връща броя на елементите на xs.
г). any' p xs, която връща дали предикатът p e верен за поне един от елементите на xs.
д). all' p xs, която връща дали предикатът p e верен за всеки от елементите на xs.
е). minimum' xs, която връща най-малкия елемент на xs.
ж). maximum' xs, която връща най-големия елемент на xs.
з). concat' xss, която приема списък от списъци xss и ги конкатенира в един общ списък.
Примери:
    sum' [1..10] -> 55
    product' [1..10] -> 3628800
    length' [1..10] -> 10
    any' even [1..10] -> True
    all' even [1..10] -> False
    minimum' [1..10] -> 1
    maximum' [1..10] -> 10
    concat' [[1], [2], [3]] -> [1, 2, 3]
-}
sum' :: Num a => [a] -> a
sum' = foldl (+) 0

product' :: Num a => [a] -> a
product' = foldl (*) 1

length' :: [a] -> Int
length' = foldl (\acc _ -> acc + 1) 0

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldl (||) False $ map f xs

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldl (&&) True $ map f xs

-- foldl1 f (x:xs) = foldl f x xs

minimum' :: Ord a => [a] -> a
minimum' = foldl1 min

maximum' :: Ord a => [a] -> a
maximum' = foldl1 max

concat' :: [[a]] -> [a]
concat' xss = foldl (++) [] xss

{-
Задача 2. Използвайте fold(l/r), за да дефинирате функцията reverse' xs,
която приема списък xs и обръща елементите му.
Примери:
    reverse' [1, 2, 3] -> [3, 2, 1]
-}

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x:acc) [] xs

{-
Задача 3. Дефинирайте функцията compose fs, която приема списък от едноаргументни функцуии
и връща тяхната композиция, т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))
Пример:
    compose [(+1), (2*)] 7 -> (2 * 7) + 1 = 15
-}

compose :: [(a -> a)] -> (a -> a)
compose fs = foldr1 (.) fs

{-
Задача 4*. Дефинирайте функцията combinations xss, която приема списък от списъци xss
и връща списък с всички възможни комбинации, съдържащи по един елемент от всеки от
подсписъците на xss.
Пример:
    combinations [[1, 2, 3], [4], [5, 6]] -> [[1,4,5], [1,4,6], [2,4,5], [2,4,6], [3,4,5], [3,4,6]]
    combinations [[1,2,3]] -> [[1],[2],[3]]
    combinations [] -> [[]]
-}

combinations :: [[a]] -> [[a]]
combinations xss = foldr (\xs yss -> [x:ys | x <- xs, ys <- yss]) [[]] xss