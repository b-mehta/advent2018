{-# LANGUAGE PartialTypeSignatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- temp

import Common
import Data.Semigroup
import Data.Array
import Data.List.Split

main :: IO ()
main = runMainT 10 parser part1

runMainT :: Int -> Parser a -> (a -> _) -> IO ()
runMainT n p p1 = do
  inp <- readFile (fileName n)
  case parse p (fileName n) inp of
    Left e -> putStr (errorBundlePretty e)
    Right xs -> do
      mapM_ putStrLn (p1 xs)

part1 :: [((Int,Int),(Int,Int))] -> _
part1 xs = map draw $ take 10 $ drop 10363 $ iterate (step vs) ls
  where (ls, vs) = unzip xs

-- 10368

step :: Velocities -> Locations -> Locations
step = zipWith add

draw :: Locations -> _
draw ls = unlines $ transpose $ chunksOf (yhi-ylo+1) $ elems $ array ((xlo,ylo),(xhi,yhi)) ([((x,y),'.') | y <- [ylo..yhi], x <- [xlo..xhi]] ++ [(t,'#') | t <- ls])
  where ((xlo,xhi),(ylo,yhi)) = minMax ls

minMax :: (Ord a, Bounded a) => [(a,a)] -> ((a,a),(a,a))
minMax = bothMap (getMin *** getMax) . foldMap (bothMap (Min &&& Max))

size :: Num a => ((a,a),(a,a)) -> (a,a)
size ((xlo,xhi),(ylo,yhi)) = (xhi-xlo, yhi-ylo)

add :: Num a => (a,a) -> (a,a) -> (a,a)
add (a,b) (c,d) = (a+c, b+d)

part2 :: [((Int,Int),(Int,Int))] -> _
part2 = const 1

type Locations = [(Int,Int)]
type Velocities = [(Int,Int)]

parser :: Parser [((Int,Int),(Int,Int))]
parser = lineParser line

number :: Parser Int
number = choice [fmap negate (char '-' *> num), num]

line :: Parser ((Int,Int),(Int,Int))
line = do
  _ <- nNum
  p1 <- number
  _ <- nNum
  p2 <- number
  _ <- nNum
  p3 <- number
  _ <- nNum
  p4 <- number
  _ <- char '>'
  return ((p1,p2),(p3,p4))
