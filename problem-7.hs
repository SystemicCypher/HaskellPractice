--Problem 7
import Base
import Data.Text hiding (zip, foldr, head)
import Prelude hiding (zip)

foldRope :: (a -> b -> b) -> b -> Rope a -> b
foldRope f z (List xs) = subfold_f xs
    where subfold_f (x:xs) = f x (subfold_f xs)
          subfold_f [] = z
foldRope f z (Concat l r) =  foldRope f (foldRope f z r) l

ropeToList :: Rope a -> [a]
ropeToList x = foldRope (:) [] x

main = do 
    s <- getRope :: IO (Rope Int)
    putStrLn $ toString (ropeToList s)