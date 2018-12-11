import Common
import Parsers
import Control.Monad.State
import qualified Data.Sequence as Seq
import qualified Data.IntMap.Strict as M

data GameState = GS Int (Seq.Seq Int) Int

main :: IO ()
main = runMainP 9 magicNums part1 part2

part1 :: (Int,Int) -> Int
part1 = maximum . uncurry result

play :: Int -> State GameState Int
play n = do
  GS s b c <- get
  if n `mod` 23 == 0
     then let place = (c - 7) `mod` s
           in do put $ GS (s - 1) (Seq.deleteAt place b) place
                 return $! n + Seq.index b place
     else let place = (c + 2) `mod` s
           in do put $ GS (s + 1) (Seq.insertAt place n b) place
                 return 0

scores :: Int -> [Int]
scores k = evalState (traverse play [1..k]) $ GS 1 (Seq.singleton 0) 0

result :: Int -> Int -> M.IntMap Int
result n = M.fromListWith (+) . zip (cycle [1..n]) . scores

part2 :: (Int,Int) -> Int
part2 = maximum . uncurry result . second (*100)
