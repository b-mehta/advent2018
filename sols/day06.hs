{-# LANGUAGE PartialTypeSignatures, TupleSections #-}

import Common

import Data.Semigroup (Min(..), Max(..))
import qualified Data.MultiSet as MS
import qualified Data.Set as S

type Point = (Int,Int)

main :: IO ()
main = runMainP 6 parser part1 part2

part1 :: [Point] -> Int
part1 ts = mostPopularValue . MS.filter (`S.notMember` infinite) $ area
  where (xR@(minX, maxX), yR@(minY, maxY)) = minMax ts
        boundary = concat [(minX,) <$> ys, (maxX,) <$> ys, (,minY) <$> xs, (,maxY) <$> xs]
        infinite = S.fromList $ mapMaybe (closest ts) boundary
        area = MS.fromList $ mapMaybe (closest ts) $ (,) <$> xs <*> ys
        xs = range' xR
        ys = range' yR

closest :: [Point] -> Point -> Maybe Point
closest points this = name <$> uniqueBest (labelWith (`manhattan` this) points)

manhattan :: Point -> Point -> Int
manhattan (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)

part2 :: [Point] -> Int
part2 ts = length . filter ((<10000) . total ts) $ (,) <$> xs <*> ys
  where (xs, ys) = bothMap range' $ minMax ts

minMax :: (Ord a, Bounded a) => [(a,a)] -> ((a,a),(a,a))
minMax = bothMap (getMin *** getMax) . foldMap (bothMap (Min &&& Max))

total :: [Point] -> Point -> Int
total points me = sum [manhattan point me | point <- points]

parser :: Parser [Point]
parser = lineParser (middle num ", " num)
