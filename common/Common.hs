module Common
  ( module Common
  , module Data.Bifunctor
  , module Data.List
  , module Data.Monoid
  , module Data.Void
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , (***), (&&&), (>>>)
  , bool
  , mapMaybe, listToMaybe, catMaybes, maybe
  , toList
  ) where

import Control.Arrow ((***), (&&&), (>>>))
import Control.Monad (join)
import Data.Bifunctor (first, second, bimap)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.MultiSet as MS
import qualified Data.Map as M

type Parser a = Parsec Void String a

-- general conveniences
fileName :: Int -> FilePath
fileName n = "input/" ++ show n ++ ".txt"

runMain :: (Show x, Show y) => Int -> (String -> a) -> (a -> x) -> (a -> y) -> IO ()
runMain n p p1 p2 = do
  inp <- readFile $ fileName n
  let parsed = p inp
  putStrLn ("Part 1: " ++ show (p1 parsed))
  putStrLn ("Part 2: " ++ show (p2 parsed))

runMainP :: (Show x, Show y) => Int -> Parser a -> (a -> x) -> (a -> y) -> IO ()
runMainP n p p1 p2 = do
  inp <- readFile (fileName n)
  case parse p (fileName n) inp of
    Left e -> putStr (errorBundlePretty e)
    Right xs -> do
      putStrLn ("Part 1: " ++ show (p1 xs))
      putStrLn ("Part 2: " ++ show (p2 xs))

-- combinators
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)
infixr ...

number :: Parser Int
number = read <$> many digitChar

lineParser :: Parser a -> Parser [a]
lineParser line = line `endBy` eol <* eof

middle :: Parser a -> String -> Parser c -> Parser (a,c)
middle a b c = (,) <$> a <*> (string b *> c)

moreThanOne :: [a] -> Bool
moreThanOne (_:_:_) = True
moreThanOne _ = False

makePairs :: [a] -> [(a,a)]
makePairs (x:y:z) = (x,y): makePairs z
makePairs [] = []
makePairs _ = error "odd pairs"

range :: Enum a => a -> a -> [a]
range x y = enumFromTo x (pred y)

-- multiset functions
data Labelled a b = Labelled { value :: a
                             , name :: b
                             }

instance Eq a => Eq (Labelled a b) where
  x == y = value x == value y

instance Ord a => Ord (Labelled a b) where
  x `compare` y = value x `compare` value y

msOccFilter :: (Int -> Bool) -> MS.MultiSet a -> MS.MultiSet a
msOccFilter p = MS.fromOccurMap . M.filter p . MS.toMap

mostPopular :: Ord a => MS.MultiSet a -> a
mostPopular = name . MS.foldOccur (max ... flip Labelled) (Labelled (-1) (error "empty set"))
