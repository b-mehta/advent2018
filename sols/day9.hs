{-# LANGUAGE PartialTypeSignatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-} -- temp

{-# LANGUAGE BangPatterns #-}

import Common
import Data.Maybe
import Control.Monad.State
import qualified Data.Sequence as Seq
-- import Debug.Trace

import qualified Data.MultiSet as MS

data GameState = GS !(Seq.Seq Int) !Int

main :: IO ()
main = runMainP 9 parser part1 part2

part1 :: (Int,Int) -> Int
part1 = maximum . MS.toMap . uncurry result

step :: Int -> GameState -> (Int, GameState)
step marble (GS board current) =
  if marble `mod` 23 == 0
     then
       let place = if current - 7 < 0
                        then current - 7 + Seq.length board
                        else current - 7
           Just killed = board Seq.!? place
           !new = Seq.deleteAt place board
        in (marble + killed, GS new place)
     else
       let place = if current + 2 > Seq.length board
                        then current + 2 - Seq.length board
                        else current + 2
        in (0, GS (Seq.insertAt place marble board) place)

step' :: Monad m => Int -> StateT GameState m Int
step' n = state (step n)

step'' :: Int -> State GameState Int
step'' n
  | n `mod` 23 == 0 = do
    GS board current <- get
    let place = current - 7 + (if current - 7 < 0 then Seq.length board else 0)
    put $! GS (Seq.deleteAt place board) place
    return $! n + (fromJust (board Seq.!? place))
  | otherwise = do
    GS board current <- get
    let place = current + 2 - (if current + 2 > Seq.length board then Seq.length board else 0)
    put $! GS (Seq.insertAt place n board) place
    return 0

initial :: GameState
initial = GS (Seq.singleton 0) 0

values :: State GameState [Int]
values = traverse step'' [1..]

scores :: Int -> [(Int, Int)]
scores n = zip (cycle [1..n]) (evalState values initial)

result :: Int -> Int -> MS.MultiSet Int
result n k = MS.fromOccurList (take k $ scores n)

part2 :: _ -> _
part2 (n,k) = maximum . MS.toMap $ result n (k*100)

parser :: Parser (Int,Int)
parser = middle number " players; last marble is worth " number <* string " points\n"

-- parser :: String -> [_]
-- parser = map line . lines
--
-- line :: String -> _
-- line = error "todo: line parser"
