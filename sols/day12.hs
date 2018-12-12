{-# LANGUAGE ParallelListComp #-}

import Common
import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Control.Comonad

type Rules = Map [Bool] Bool
type Input = (NonEmpty Bool, Rules)

main :: IO ()
main = runMainP 12 parser part1 part2

part2 :: Input -> [(Int,Int)]
part2 (start, r) = take 501 [(n, run $ zip [(-2)*n..] $ NE.toList t) | t <- iterate (step r) start | n <- [0..]]

part1 :: Input -> Int
part1 = snd . (!!20) . part2

run :: [(Int,Bool)] -> Int
run = sum . map fst . filter snd

step :: Rules -> NonEmpty Bool -> NonEmpty Bool
step r = extend (solve r) . ((False :| [False, False, False]) <>)

solve :: Rules -> NonEmpty Bool -> Bool
solve r = (r !) . shape . NE.toList

shape :: [Bool] -> [Bool]
shape xs = take 5 (xs ++ repeat False)

parser :: Parser Input
parser = do
  start <- string "initial state: " *> NE.some1 thing <* eol <* eol
  rest <- lineParser line
  return (start, M.fromList rest)

line :: Parser ([Bool], Bool)
line = do
  start <- many thing
  goal <- string " => " *> thing
  return (start, goal)

thing :: Parser Bool
thing = True <$ char '#' <|> False <$ char '.'
