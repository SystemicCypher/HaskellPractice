--Problem 2
import Base
import Data.Text hiding (zip, foldr)
import Prelude hiding (zip)

zip  :: [a] -> [b] -> [(a,b)]
zip = foldr next (const [])
    where next a f (b:bs) = (a,b):(f bs)
          next a f [] = []

main = do 
    l <- getList :: IO [Text]
    r <- getList :: IO [Text]
    x <- getList :: IO [Text]
    y <- getList :: IO [Text]
    putStrLn $ toString (zip l r)
    putStrLn $ toString (zip x y)