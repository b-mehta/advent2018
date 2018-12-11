import Common

main :: IO ()
main = mapM_ print (part2 9435)

part2 :: Int -> [Labelled Int (Int,Int,Int)]
part2 s = localMaxima $ labelWith (totalPower s) $ (,,) <$> [1..298] <*> [1..298] <*> [1..298]

cellScore :: Int -> (Int,Int) -> Int
cellScore s (x,y) = hundreds (((x+10)*y + s)*(x+10)) - 5

totalPower :: Int -> (Int,Int,Int) -> Int
totalPower s (w,x,y) = sum [cellScore s (x+a,y+b) | a <- [0..w-1], b <- [0..w-1]]

hundreds :: Int -> Int
hundreds x = (x `div` 100) `mod` 10

localMaximaFrom :: Ord a => a -> [a] -> [a]
localMaximaFrom x (y:ys)
  | x >= y = localMaximaFrom x ys
  | otherwise = y:localMaximaFrom y ys
localMaximaFrom _ [] = []

localMaxima :: Ord a => [a] -> [a]
localMaxima (x:xs) = localMaximaFrom x xs
localMaxima [] = error "empty"
