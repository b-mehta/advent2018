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
import Data.MultiSet (toMap, fromOccurMap, MultiSet)
import qualified Data.Map as M (filter)

type Parser a = Parsec Void String a

runMain :: (Show x, Show y) => Int -> (String -> a) -> (a -> x) -> (a -> y) -> IO ()
runMain n p p1 p2 = do
  inp <- readFile $ fileName n
  let parsed = p inp
  putStrLn ("Part 1: " ++ show (p1 parsed))
  putStrLn ("Part 2: " ++ show (p2 parsed))

fileName :: Int -> FilePath
fileName n = "input/" ++ show n ++ ".txt"

runMainP :: (Show x, Show y) => Int -> (Parsec Void String a) -> (a -> x) -> (a -> y) -> IO ()
runMainP n p p1 p2 = do
  inp <- readFile (fileName n)
  case parse p (fileName n) inp of
    Left e -> putStr (errorBundlePretty e)
    Right xs -> do
      putStrLn ("Part 1: " ++ show (p1 xs))
      putStrLn ("Part 2: " ++ show (p2 xs))

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)
infixr ...

number :: Parser Int
number = read <$> many digitChar

lineParser :: Parser a -> Parser [a]
lineParser line = line `endBy` eol <* eof

bothMap :: (a -> b) -> (a,a) -> (b,b)
bothMap = join bimap

moreThanOne :: [a] -> Bool
moreThanOne (_:_:_) = True
moreThanOne _ = False

msOccFilter :: (Int -> Bool) -> MultiSet a -> MultiSet a
msOccFilter p = fromOccurMap . M.filter p . toMap

middle :: Parser a -> String -> Parser c -> Parser (a,c)
middle a b c = (,) <$> a <*> (string b *> c)
