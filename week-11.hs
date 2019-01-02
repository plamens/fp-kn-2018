import Data.Char

main = do
    print $ digits 1234
    print $ digits' 1234
    print $ title "tHe SoUND AND the furY"
    print $ title' "tHe SoUND AND the furY"
{-
(Int, String, [Int])
(1, "sdf", [1,2,3])

fst (1, "s") == 1
snd (1, "s") == "s"

zip [1..7] ['a'..'c'] = [(1,'a'),(2,'b'),(3,'c')]
zipWith (^) [1..6] [10, 20, 30] == [1,1048576,205891132094649]

-}

digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = let firstDigits = n `div` 10
                      lastDigit = n `mod` 10
                  in digits firstDigits ++ [lastDigit]

digits' n
    | n < 10 = [n]
    | otherwise = digits firstDigits ++ [lastDigit]
        where firstDigits = n `div` 10
              lastDigit = n `mod` 10

{-
[x^2 | x <- [1..10], even x] == map (^2) (filter even [1..10])
[(x, y) | x <- [1..5], y <- ['a','b'], even x] == [(2,'a'),(2,'b'),(4,'a'),(4,'b')]
-}

{-
Задача 1. Дефинирайте функция title str,
която превръща първата буква на всяка дума в str в главна,
а всички останали в малки.
пример:
title "tHe SoUND AND the furY" = The Sound And The Fury

(import Data.Char за toLower и toUpper)
-}

title :: String -> String
title str = helper str True
    where helper "" _ = ""
          helper (c:str) capitalize
            | c == ' ' = c : helper str True
            | capitalize = toUpper c : helper str False
            | otherwise = toLower c : helper str False

title' str = unwords $ map capitalize $ words str
    where capitalize "" = ""
          capitalize (c:str) = toUpper c : map toLower str