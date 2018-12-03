import Common
import qualified Data.MultiSet as MS

main :: IO ()
main = runMain 2 lines part1 part2

part1 :: Ord a => [[a]] -> Int
part1 = uncurry (*) . bothMap getSum . foldMap checkSum

checkSum :: Ord a => [a] -> (Sum Int, Sum Int)
checkSum = bothMap (Sum . fromEnum . getAny) . foldMap hasNum . MS.toMap . MS.fromList

hasNum :: Int -> (Any, Any)
hasNum n = bothMap Any (n==2, n==3)

part2 :: Eq a => [[a]] -> [a]
part2 s = head [t | (x:xs) <- tails s, y <- xs, t <- toList (good x y)]

good :: Eq a => [a] -> [a] -> Maybe [a]
good xs ys = case neq of
               _:_:_ -> Nothing
               _ -> Just (fst <$> eq)
  where (eq, neq) = partition (uncurry (==)) $ zip xs ys
