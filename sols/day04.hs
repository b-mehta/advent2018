import Common
import Data.List.Split
import qualified Data.MultiSet as MS

type Record = (Int, [(Int, Int)])

main :: IO ()
main = runMain 4 parser part1 part2

part1 :: [Record] -> Int
part1 rs = mostPopular popTimes * guard
  where guard = mostPopular . MS.fromOccurList . map (second getT) $ rs
        getT ts = sum [y-x | (x,y) <- ts]
        popTimes = MS.fromList [x | (g,ts) <- rs, g == guard, x <- ts >>= range]

part2 :: [Record] -> Int
part2 = uncurry (*) . mostPopular . MS.unions . map night
  where night (r, ts) = MS.fromDistinctAscList $ (r,) <$> (ts >>= range)

parser :: String -> [Record]
parser = map makeRecord . split strat . sort . lines

strat :: Splitter String
strat = dropInitBlank . keepDelimsL . whenElt $ isInfixOf "Guard"

makeRecord :: [String] -> Record
makeRecord (x:xs) = (guard, makePairs times)
  where guard = read . tail $ words x !! 3
        times = read . take 2 . drop 15 <$> xs
makeRecord [] = error "bad guard"
