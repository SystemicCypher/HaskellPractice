--Problem 1
import Base
import Data.Text hiding (intercalate, map)

indirectMap :: (a -> b) -> [[a]] -> [[b]]
indirectMap f x = map (map ( f ) ) x

double x = x * 2

main = do 
    l <- getList :: IO [[Int]]
    s <- getList :: IO [[Text]]
    putStrLn $ toString (indirectMap double l)
    putStrLn $ toString (indirectMap Data.Text.reverse s)


