--Problem 8
import Base
import Data.Text
import Data.Text.IO 
import Data.List.Split
import Data.List
import Data.Maybe

mapReduce :: ((a, b) -> (c, d)) -> ((c, [d]) -> (c, d)) -> [(a, b)] -> [(c, d)]
mapReduce f g z = Prelude.map g (Prelude.foldr grouping [] (Prelude.map f z))


grouping :: (String, Int) -> [(String, [Int])] -> [(String, [Int])]
grouping x z = 
    if (isJust indX)  
        then (fst splitList ++ [(fst x, (snd x) : (snd (Prelude.head (snd splitList))) )] ++ (Prelude.tail (snd splitList)))
        else (fst x, [(snd x)]) : z
        where splitList = Prelude.splitAt indeX z
              indX = Data.List.findIndex (==(fst x)) zn
              zn = fst (unzip z)
              indeX = fromJust indX

mapper :: (Text, Int) -> (String, Int)
mapper t = (toString (fst t) ,(snd t)) --A little forced

reducer :: (String, [Int]) -> (String, Int)
reducer t = (fst t, Prelude.length (snd t)) --technically hack-y but it's not *incorrect* 




--Utility stuff

splits = Data.Text.splitOn (pack " ")

pairer :: Text -> [(Text, Int)]
pairer str = Prelude.foldr (doubleapp pair (:)) [] (splits str)
    where pair x = (x , Data.Text.length x)
          doubleapp f g y = g (f y) 



--Not sure if needed, but it helps ^



main = do 
    s <- Data.Text.IO.getLine :: IO Text
    print $ mapReduce mapper reducer (pairer s) 