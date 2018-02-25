--Problem 3

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap g (x:xs) = (g x) ++ (flatMap g xs)
flatMap g [] = []


fibonacci :: Int -> [Int]
fibonacci 0 = [1]
fibonacci 1 = [1,1]
fibonacci n = head(fibonacci(n-1)) + head(fibonacci(n-2)) : fibonacci(n-1)

characters :: String -> [Char]
characters (x:xs) = x : characters xs
characters [] = []


