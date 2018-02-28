--Problem 4
import Base
import Data.Text hiding (zip, foldr, head)
import Prelude hiding (zip)

--data Rope a = List [a] | Concat (Rope a) (Rope a) 
--    deriving (Eq, Show)

mapRope :: (a -> b) -> Rope a -> Rope b
mapRope f (List xs) = List(xs_map xs)
    where  xs_map (x:xs) = f x : xs_map xs
           xs_map [] = []
mapRope f (Concat l r) = Concat (mapRope f l) (mapRope f r) 

double x = x * 2


main = do 
    s <- getRope :: IO (Rope Int)
    l <- getRope :: IO (Rope Text)
    putStrLn $ toString (mapRope double s)
    putStrLn $ toString (mapRope Data.Text.reverse l)