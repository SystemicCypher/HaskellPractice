--Problem 4
data Rope a = List [a] | Concat (Rope a) (Rope a) 
    deriving (Eq, Show)

mapRope :: (a -> b) -> Rope a -> Rope b
mapRope f (List xs) = List(xs_map xs)
    where  xs_map (x:xs) = f x : xs_map xs
           xs_map [] = []
mapRope f (Concat l r) = Concat (mapRope f l) (mapRope f r) 


