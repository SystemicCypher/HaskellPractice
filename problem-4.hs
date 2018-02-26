--Problem 4
data Rope a = List a | Concat (Rope a) (Rope a) 
mapRope :: (a -> b) -> Rope a -> Rope b
mapRope f r = 