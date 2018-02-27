--Problem 8
mapReduce :: ((a, b) -> (c, d)) -> ((c, [d]) -> (c, d)) -> [(a, b)] -> [(c, d)]
mapReduce f g xs = undefined


mapper ::  (a, b) -> (c, d)
mapper t = 