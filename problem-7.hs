--Problem 7
data Rope a = List [a] | Concat (Rope a) (Rope a) 
    deriving (Eq, Show)

foldRope :: (a -> b -> b) -> b -> Rope a -> b
foldRope f z (List xs) = subfold_f xs
    where subfold_f (x:xs) = f x (subfold_f xs)
          subfold_f [] = z
foldRope f z (Concat l r) =  foldRope f (foldRope f z r) l

ropeToList :: Rope a -> [a]
ropeToList x = foldRope (:) [] x