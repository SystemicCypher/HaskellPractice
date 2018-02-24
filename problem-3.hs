--Problem 3

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap g (a:as) = flattener(g a : flatMap g as )
flatMap g [] = []
where flattener = 


fibonacci :: Int -> [Int]
fibonacci 0 = [1]
fibonacci 1 = [1,1]
fibonacci n = head(fibonacci(n-1)) + head(fibonacci(n-2)) : fibonacci(n-1)

--characters :: [String] -> [Char]
--characters [] = []
--characters str_list =  head(str_list) :  characters(tail(str_list))


--can use exp recursion on this