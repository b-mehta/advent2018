module Common
  ( module Data.Bifunctor
  , module Data.List
  , module Data.Monoid
  , module Data.Void
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Data.Char
  , (***), (&&&), (>>>)
  , bool
  , mapMaybe, listToMaybe, catMaybes, maybe, fromMaybe
  , toList
  , Parser, runMain, runMainP, lineParser, anyNum, nNum, num, (...), bothMap, moreThanOne, makePairs
  , range, range', middle, minMax2d, uniqueBest, labelWith, msOccFilter, mostPopular', mostPopular
  , mostPopularValue, Labelled(..), S(..)
  ) where

import Control.Arrow hiding (first, second)
import Control.Monad
import Data.Bifunctor (first, second, bimap)
import Data.Bool (bool)
import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Void
import Text.Megaparsec hiding (match, State)
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
  putStrLn ("Part 1:\n" ++ show (p1 parsed))
  putStrLn ("Part 2:\n" ++ show (p2 parsed))

runMainP :: (Show x, Show y) => Int -> Parser a -> (a -> x) -> (a -> y) -> IO ()
runMainP n p p1 p2 = do
  inp <- readFile (fileName n)
  case parse p (fileName n) inp of
    Left e -> putStr (errorBundlePretty e)
    Right xs -> do
      putStrLn ("Part 1:\n" ++ show (p1 xs))
      putStrLn ("Part 2:\n" ++ show (p2 xs))

-- combinators
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)
infixr ...

bothMap :: (a -> b) -> (a,a) -> (b,b)
bothMap = join bimap

-- parser helpers
num :: Parser Int
num = read <$> many digitChar

anyNum :: Parser Int
anyNum = (negate <$ char '-' <|> pure id <|> id <$ char '+') <*> num

lineParser :: Parser a -> Parser [a]
lineParser line = line `endBy` eol <* eof

middle :: Parser a -> String -> Parser c -> Parser (a,c)
middle a b c = (,) <$> a <*> (string b *> c)

nNum :: Parser String
nNum = many . satisfy $ (\x -> isLetter x || x `elem` "!@$%^&*()=_<>[];.,# ")

-- list functions
moreThanOne :: [a] -> Bool
moreThanOne (_:_:_) = True
moreThanOne _ = False

makePairs :: [a] -> [(a,a)]
makePairs (x:y:z) = (x,y): makePairs z
makePairs [] = []
makePairs _ = error "odd pairs"

range :: Enum a => (a, a) -> [a]
range (x,y) = enumFromTo x (pred y)

range' :: Enum a => (a, a) -> [a]
range' (x,y) = enumFromTo x y

minMax2d :: (Ord a, Bounded a, Foldable f) => f (a,a) -> ((a,a),(a,a))
minMax2d = bothMap (getMin *** getMax) . foldMap (bothMap (Min &&& Max))

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

newtype Min2' a = Min2' [a]

firstTwo :: [a] -> Min2' a
firstTwo = Min2' . take 2

instance Ord a => Semigroup (Min2' a) where
  Min2' x <> Min2' y = firstTwo (merge x y)

instance Ord a => Monoid (Min2' a) where
  mempty = Min2' []

uniqueBest :: Ord a => [a] -> Maybe a
uniqueBest t = case foldMap (Min2' . (:[])) t of
                 Min2' [x] -> Just x
                 Min2' [x,y] | x /= y -> Just x
                 _ -> Nothing

data Labelled a b = Labelled { value :: a
                             , name :: b
                             }
                             deriving Show

instance Eq a => Eq (Labelled a b) where
  x == y = value x == value y

instance Ord a => Ord (Labelled a b) where
  x `compare` y = value x `compare` value y

labelWith :: Functor f => (b -> a) -> f b -> f (Labelled a b)
labelWith f = fmap (\t -> Labelled (f t) t)

newtype S = S String
instance Show S where
  show (S s) = s

-- multiset functions
msOccFilter :: (Int -> Bool) -> MS.MultiSet a -> MS.MultiSet a
msOccFilter p = MS.fromOccurMap . M.filter p . MS.toMap

mostPopular' :: Ord a => MS.MultiSet a -> Labelled Int a
mostPopular' = MS.foldOccur (max ... flip Labelled) (Labelled (-1) (error "empty set"))

mostPopular :: Ord a => MS.MultiSet a -> a
mostPopular = name . mostPopular'

mostPopularValue :: Ord a => MS.MultiSet a -> Int
mostPopularValue = value . mostPopular'
