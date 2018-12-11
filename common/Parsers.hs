{-# LANGUAGE FlexibleInstances #-}
module Parsers
  ( magicNums
  , afterNum
  , surroundNum
  ) where

import Common

afterNum :: Parser Int
afterNum = anyNum <* nNum

surroundNum :: Parser Int
surroundNum = nNum *> anyNum <* nNum

class MagicParser a where
  magicNums :: Parser a

instance MagicParser Int where
  magicNums = surroundNum

instance MagicParser (Int,Int) where
  magicNums = (,) <$> surroundNum <*> afterNum

instance MagicParser (Int,Int,Int) where
  magicNums = (,,) <$> surroundNum <*> afterNum <*> afterNum

instance MagicParser (Int,Int,Int,Int) where
  magicNums = (,,,) <$> surroundNum <*> afterNum <*> afterNum <*> afterNum

instance MagicParser (Int,Int,Int,Int,Int) where
  magicNums = (,,,,) <$> surroundNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum

instance MagicParser (Int,Int,Int,Int,Int,Int) where
  magicNums = (,,,,,) <$> surroundNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum

instance MagicParser (Int,Int,Int,Int,Int,Int,Int) where
  magicNums = (,,,,,,) <$> surroundNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum

instance MagicParser (Int,Int,Int,Int,Int,Int,Int,Int) where
  magicNums = (,,,,,,,) <$> surroundNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum <*> afterNum
