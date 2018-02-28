--Problem 3
import Base
import Data.Text hiding (zip, foldr, head)
import Prelude hiding (zip)

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap g (x:xs) = (g x) ++ (flatMap g xs)
flatMap g [] = []


fibonacci :: Int -> [Int]
fibonacci 0 = [1]
fibonacci 1 = [1,1]
fibonacci n = head(fibonacci(n-1)) + head(fibonacci(n-2)) : fibonacci(n-1)

fibo :: Int -> [Int]
fibo n = Prelude.reverse (fibonacci n)

--unneeded characters function. I guess it doesn't really do anything considering String is [Char]
characters :: String -> [Char]
characters (x:xs) = x : characters xs
characters [] = []


main = do 
    s <- getList :: IO [Text]
    l <- getList :: IO [Int]
    putStrLn $ toString (flatMap splitText s)
    putStrLn $ toString (flatMap fibo l)
    


