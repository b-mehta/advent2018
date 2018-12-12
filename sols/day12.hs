{-# LANGUAGE PartialTypeSignatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- temp

import Common
import Parsers
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import qualified Data.List.NonEmpty as NE
import Data.Array
import Control.Comonad

type Rules = M.Map [Bool] Bool
type Input = ([Bool], Rules)

main :: IO ()
main = runMainP 12 parser part1 part2

part1 :: _ -> _
part1 (start, r) = zip [0..] $ take 501 [run $ zip [n..] $ NE.toList t | (t, n) <- iterate (step r *** (subtract 2)) (NE.fromList start, 0)]

run :: [(Int,Bool)] -> Int
run xs = sum [x | (x,t) <- xs, t]

step :: Rules -> NE.NonEmpty Bool -> NE.NonEmpty Bool
step r = extend (solve r) . (False NE.<|) . (False NE.<|) . (False NE.<|) . (False NE.<|)

solve :: Rules -> NE.NonEmpty Bool -> Bool
solve r = (r M.!) . shape . NE.toList

shape :: [Bool] -> [Bool]
shape (a:b:c:d:e:_) = [a,b,c,d,e]
shape xs = take 5 $ xs ++ repeat False

part2 :: _ -> _
part2 = const 0

parser :: Parser Input
parser = do
  _ <- string "initial state: "
  start <- things <* eol <* eol
  rest <- lineParser line
  return (start, makeMap rest)

makeMap :: [([Bool], Bool)] -> Rules
makeMap = M.fromList

line :: Parser ([Bool], Bool)
line = do
  start <- things
  _ <- string " => "
  goal <- thing
  return (start, goal)

thing :: Parser Bool
thing = (True <$ char '#') <|> (False <$ char '.')

things :: Parser [Bool]
things = many thing
