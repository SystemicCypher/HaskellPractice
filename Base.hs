{-# LANGUAGE FlexibleInstances #-}
module Base
  (Parsable(..), Rope(..), ropeAdd, parse, getList, getRope, splitText) where

import Data.Text hiding (intercalate, map)
import Data.List
import Text.Parsec.Text
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Control.Applicative ((<$>), (<$), (<*>))


-- We are defining our own S-expression parser here, for a couple
-- of types we want to be able to parse.
-- This is not a general-purpose s-expression parser, it's specific to this
-- assignment. You will probably need to install parsec:
-- $ cabal update && cabal install parsec

-- You don't need to understand what happens up until the Rope type definition.
-- However, if you want to:
--  * We are defining a type class of Parsable types, which has two methods:
--     -- parser returns a parser for that type
--     -- toString prints the value of that type, in such a way that it can be parsed again with its parser
--
--  * We then define instances of this class, for Int, Text, [a] for every a which is itself Parsable, Rope a, and
--    (a, b), where a and b are both parsable.
--  * We then go on to define some helper functions which may come in handy.
class Parsable a where
  parser :: Parser a
  toString :: a -> String

pSign =   negate <$ char '-'
      <|> id <$ optional (char '+')

instance Parsable Int where
  parser = do
    s <- pSign
    num <- many1 digit
    return $ s (read num)

  toString = show

instance Parsable Char where
  parser = alphaNum 
  toString = show

instance Parsable Bool where
  parser =   True  <$ string "true"
         <|> False <$ string "false"
  
  toString True  = "true"
  toString False = "false"

instance (Parsable a, Parsable b) => Parsable (a, b) where
  parser = (,) <$> (spaces >> parser) <*> (spaces >> parser)

  toString (x, y) = (toString x) ++ " " ++ (toString y)

instance Parsable Text where
  parser = pack <$> many1 alphaNum

  toString = unpack

command :: String -> Parser a -> Parser a
command cmd p = do
  try (spaces >> char '(' >> spaces >> string cmd)
  a <- p
  char ')'
  return a

instance (Parsable a) => Parsable [a] where
  parser = command "list" (many (try $ spaces >> parser))

  toString xs = "(list " ++ elems ++ ")"
    where elems = intercalate " " (fmap toString xs)

-- This is your Rope data structure. The two construcotrs are List and Concat.
data Rope a = List [a] | Concat (Rope a) (Rope a)
  deriving (Eq, Show)

exampleRope = Concat (List [1, 2, 3]) (Concat (List [4, 5]) (List [6, 7]))

-- As an example, we'll define a function that adds an element to the begining of the rope
-- This essentially means we need to find the left-most list inside a rope and add an element to it
-- This is only a demonstration of how to use a rope structure.
ropeAdd :: a -> Rope a -> Rope a
ropeAdd a (List as)    = List (a:as)
ropeAdd a (Concat l r) = Concat (ropeAdd a l) r

-- We need to parse ropes, (list 1 2 3) will correspond to a list, and
-- (+ arg1 arg2) will correspond to (Concat arg1 arg2), where arg1 and arg2
-- are other ropes
instance (Parsable a) => Parsable (Rope a) where
  parser =   command "list" (List <$> many (try $ spaces >> parser))
         <|> command "+" (Concat <$> parser <*> parser)
  
  toString (List x) = toString x
  toString (Concat l r) = "(+ " ++ (toString l) ++ " " ++ (toString r) ++ ")"

parse :: (Parsable a) => String -> Maybe a
parse str = case (Parsec.parse parser "" (pack str)) of
  Left _ -> Nothing
  Right x -> Just x

getList :: (Parsable a) => IO [a]
getList = do
  line <- getLine
  case (parse line) of
    Just x  -> return x
    Nothing -> error "Failed to parse list"

getRope :: (Parsable a) => IO (Rope a)
getRope = do
  line <- getLine
  case (parse line) of
    Just x  -> return x
    Nothing -> error "Failed to parse rope"

-- Completely nonsensical example function
-- switches all lists from left to right inside a concat
swapRopes (List xs)      = List xs
swapRopes (Concat xs ys) = Concat ys xs

-- You can use this to split a Text string into a list of single-character strings.
-- You will need this for your flatMap implementation
splitText :: Text -> [Text]
splitText s = fmap singleton $ unpack s

-- How you might load a list and a rope from input
main = do
  l <- getList :: IO [Int]
  putStrLn $ toString (map (*10) l)
  
  r <- getRope :: IO (Rope Text)
  putStrLn $ toString (swapRopes r)
