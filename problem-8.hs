--Problem 8
import Base
import Data.Text
import Data.Text.IO 
import Data.List.Split
import Data.List
import Data.Maybe

mapReduce :: (Eq c) => ((a, b) -> (c, d)) -> ((c, [d]) -> (c, d)) -> [(a, b)] -> [(c, d)]
mapReduce f g z = Prelude.map g grouped
    where lists = Prelude.map f z
          grouped = Prelude.foldr grouping [] lists

--Just a helper function
--Case sensitive though...not sure if it needs to be insensitive
grouping :: (Eq c) => (c, d) -> [(c, [d])] -> [(c, [d])]
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

stringify :: (String, Int) -> String
stringify (word, count) = "" ++ word ++ ": " ++ show count 

main = do 
    s <- Data.Text.IO.getLine :: IO Text
    Prelude.putStrLn $ Data.List.unlines ( Data.List.map stringify (mapReduce mapper reducer (pairer s))) 