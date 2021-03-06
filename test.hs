add :: Int -> Int -> Int
add a b =
    a + b

rep :: (Int->Int->Int) -> Int -> Int -> Int
rep f n x =
    if n == 1
        then x
        else f (rep f (n-1) x)  x

doubleArgu :: (Int -> Int -> Int) -> Int -> Int
doubleArgu f a = 
   f a a

mult = rep add
square = doubleArgu mult

double = mult 2
