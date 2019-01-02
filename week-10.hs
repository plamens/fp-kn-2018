main = do
    print (times 2 7)
    print (7 `times` 2)
    print (times' 2 7)
    print (merge [1, 3, 7] [2, 4, 6])
{-
Задача 1. Да се дефинира функция times n x,
която получава цяло положително число n и стойност
от произволен тип x и връща списък съдържащ x, повторено n пъти.
Примери:
    2 `times` 7 = [7, 7]
    times 2 7
    7 `times` 2 = [2, 2, 2, 2, 2, 2, 2]
-}

times :: Int -> a -> [a]
times 0 x = []
times n x = x : times (n-1) x

times' n x = if n==0
    then []
    else x : times' (n-1) x

{-
Задача 2. Нaпишете функцията merge xs ys, която приема два списъка подредени в нарастващ ред и ги
обединява в един списък, чийто елементи също са подредени в нарастващ ред.
    Пример: merge [1, 3, 7] [2, 4, 6] = [1, 2, 3, 4, 6, 7]
-}
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
    if x < y
        then x : merge xs (y:ys)
        else y : merge (x:xs) ys

{-
mergeWith5 = merge [5]
mergeWith5 [4] == [4,5]

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
add3 = (\x y z -> x + y + z)
add3 = (\x -> (\y -> (\z -> x + y + z)))

doubleList = map (\x -> x+x)
doubleList [1..5] == [2,4,6,8,10]

map (\x -> x + 5) [1..3] == [6,7,8]
map (+1) [1..5] == [2,3,4,5,6]
map (2^) [1..5] == [2,4,8,16,32]

filter even [1..10] == [2,4,6,8,10]
filter (>5) [1..10] == [6,7,8,9,10]

zip [1,2,3] [7,8,9] == [(1,7),(2,8),(3,9)]
-}