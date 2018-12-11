import Common
import Parsers
import qualified Data.Set as S

main :: IO ()
main = runMainP 10 parser (S . part1) part2

solve :: [((Int,Int),(Int,Int))] -> (Int, [(Int,Int)])
solve xs = name . finalDrop . labelWith (size . minMax2d . snd) . zip [0..] $ iterate (zipWith add vs) ls
  where (ls, vs) = unzip xs

part1 :: [((Int,Int),(Int,Int))] -> String
part1 = draw . S.fromList . snd . solve
part2 :: [((Int,Int),(Int,Int))] -> Int
part2 = fst . solve

finalDrop :: Ord a => [a] -> a
finalDrop (x:y:xs)
  | y > x = x
  | otherwise = finalDrop (y:xs)
finalDrop [x] = x
finalDrop _ = error "none found"

draw :: (Ord a, Bounded a, Enum a) => S.Set (a,a) -> String
draw ls = unlines [[bool ' ' '#' ((x,y) `S.member` ls) | x <- range' xr] | y <- range' yr]
  where (xr,yr) = minMax2d ls

size :: Num a => ((a,a),(a,a)) -> (a,a)
size = bothMap (uncurry subtract)

add :: Num a => (a,a) -> (a,a) -> (a,a)
add (a,b) (c,d) = (a+c, b+d)

parser :: Parser [((Int,Int),(Int,Int))]
parser = lineParser $ (,) <$> magicNums <*> magicNums
