main = do
    print (isPrime 6)
    print (isPrime 7)
    print (digits 3456)

-- one line comment

{-
True
False
mod 12 5
12 `mod` 5
-}


isPrime :: Int -> Bool
isPrime n = n > 1 && isPrimeHelper' n 2

isPrimeHelper n k =
    if k >= floor (sqrt (fromIntegral n))
        then True
        else if n `mod` k == 0
            then False
            else isPrimeHelper n (k+1)


isPrimeHelper' n k
    | k >= n = True
    | n `mod` k == 0 = False
    | otherwise = isPrimeHelper n (k+1)

{-
[Int]
String = [Char]
[[Int]]

[1, 4, 5]
[1 .. 5] = [1 .. 5]
[1, 3 .. 10]
[1 ..]
['a'..'z']
123 : [3, 4, 5] == [123, 3, 4, 5]
[2,3] ++ [6,7] == [2,3,6,7]
take 5 [1,20] == [1,2,3,4,5]
drop 10 [1 .. 20] == [11,12,13,14,15,16,17,18,19,20]
reverse [1,2,3] == [3,2,1]
elem 1 [1,2,3] == True
[1,2,3] !! 1 == 2

1234 `mod` 10 == 4
1234 `div` 10 == 123
not True == False
-}
xs = [1,2,3]

{-
Задача 2. Да се дефинира функция digits n, която връща списък с цифрите на
цялото число n >= 0.
Примери:
    digits 1234 = [1, 2, 3, 4]
    digits 1750 = [1, 7, 5, 0]
-}

digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- length' :: [b] -> Int

-- append' :: [a] -> [a] -> [a]