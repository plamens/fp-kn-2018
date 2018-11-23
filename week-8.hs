main = do
    print "haskell"
    print (f 5 6)
    print (fact2 5)
    print (fib 5)
    print (take 10 xs)
    print (fib2 5 1 1 1)
    print (add2AndSquare 5)

f :: Int -> Int -> Int
f x y = x + y

fact n = if n == 0
    then 1
    else n * fact (n - 1)

fact2 0 = 1
fact2 n = n * fact (n - 1)

first (x:xs) = x
rest (x:xs) = xs

x :: Int
x = 5

xs = [1..]

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib2 n x y i = if i == n
                    then x
                    else fib2 n (x+y) x (i+1)

add2 x = x + 2
square x = x * x
add2AndSquare x = (square . add2) x