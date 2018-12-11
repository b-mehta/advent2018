import Common
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro.Platform

type Graph a = Map.Map a [a]

main :: IO ()
main = runMainP 7 parser (S . part1) part2'

parser :: Parser [(Char, Char)]
parser = lineParser line

line :: Parser (Char,Char)
line = (,) <$> (string "Step " *> upperChar <* string " must be finished before step ") <*> (upperChar <* string " can begin.")

part1 :: [(Char,Char)] -> String
part1 = topSort . toAdjList

toAdjList :: [(Char,Char)] -> Graph Char
toAdjList xs = Map.fromListWith (++) [(x,[y]) | (y,x) <- xs] `Map.union` Map.fromList [(t,[]) | t <- ['A'..'Z']]

topSort :: (Show a, Ord a) => Graph a -> [a]
topSort g = unfoldr (runStateT task) (Set.empty, g)

task :: Ord a => StateT (Set.Set a, Graph a) Maybe a
task = do
  cleanUp
  t <- zoom _1 findTask
  _2 %= Map.map (delete t)
  return t

cleanUp :: (Monad m, Ord a) => StateT (Set.Set a, Graph a) m ()
cleanUp = do
  orphans <- zoom _2 $ state (Map.partition null)
  _1 %= Set.union (Map.keysSet orphans)

findTask :: Ord a => StateT (Set.Set a) Maybe a
findTask = StateT Set.minView

type WorkersState a = [(Int, a)]
type Information a = (WorkersState a, (Set.Set a, Graph a), Int)

fixAvailable :: (Monad m, Ord a) => [a] -> StateT (Set.Set a, Graph a) m ()
fixAvailable xs = do
  _2 %= Map.map (\\ xs)
  cleanUp

completeTasks' :: (Monad m, Ord a) => StateT (Information a) m ()
completeTasks' = do
  done <- zoom _1 finishedTasks'
  zoom _2 $ fixAvailable done

finishedTasks' :: (Monad m) => StateT (WorkersState a) m [a]
finishedTasks' = do
  done <- state (partition ((==0) . fst))
  return (snd <$> done)

tryFindTask :: (Monad m, Ord a) => StateT (Set.Set a, Graph a) m (Maybe a)
tryFindTask = Set.lookupMin <$> (_1 <<%= Set.deleteMin)

assignTask' :: Monad m => StateT (Information Char) m ()
assignTask' = do
  new <- zoom _2 tryFindTask
  case new of
    Just t -> _1 %= ((fromEnum t - 4, t):)
    Nothing -> return ()

step :: Monad m => StateT (Information Char) m (Maybe Int)
step = do
  _1 %= map (first (subtract 1))
  completeTasks'
  busy <- zoom _1 $ gets length
  replicateM_ (5 - busy) assignTask'
  end <- zoom _1 $ gets null
  if end
     then preuse _3
     else _3 += 1 >> return Nothing

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = go where go = m >>= maybe go return

part2' :: [(Char,Char)] -> Int
part2' g = evalState (untilJust step) ([], (Set.empty, toAdjList g), 0)
