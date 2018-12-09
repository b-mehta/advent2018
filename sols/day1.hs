import Common
import qualified Data.IntSet as IS

main :: IO ()
main = runMain 1 parser part1 part2

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = go IS.empty . scanl (+) 0 . cycle

go :: IS.IntSet -> [Int] -> Int
go seen (x:xs)
  | x `IS.member` seen = x
  | otherwise          = go (IS.insert x seen) xs
go _ [] = error "no repeat"

parser :: String -> [Int]
parser = map number . lines

number :: String -> Int
number = read . filter (/='+')
