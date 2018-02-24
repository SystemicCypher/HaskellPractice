--Problem 2
zip'  :: [a] -> [b] -> [(a,b)]
zip' = foldr next (const [])
    where next a f (b:bs) = (a,b):(f bs)
          next a f [] = []