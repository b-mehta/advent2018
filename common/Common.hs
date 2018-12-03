module Common
  ( module Common
  , module Data.Bifunctor
  , module Data.List
  , module Data.Monoid
  , module Text.Megaparsec
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
import Text.Megaparsec

runMain :: (Show x, Show y) => Int -> (String -> a) -> (a -> x) -> (a -> y) -> IO ()
runMain n p p1 p2 = do
  inp <- readFile ("input/" ++ show n ++ ".txt")
  let parsed = p inp
  putStrLn ("Part 1: " ++ show (p1 parsed))
  putStrLn ("Part 2: " ++ show (p2 parsed))

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)
infixr ...

bothMap :: (a -> b) -> (a,a) -> (b,b)
bothMap = join bimap
