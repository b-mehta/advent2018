{-# LANGUAGE PartialTypeSignatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- temp

import Common
import Parsers
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import qualified Data.List.NonEmpty as NE
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.MultiSet (MultiSet)
import Debug.Trace

type Input = (Map (Int,Int) Track, [Cart])
data Track = Hori | Vert | For | Back | Inter
  deriving Show
data Facing = U | L | D | R
  deriving Show
data Dir = Lef | Straight | Righ
  deriving Show
data Cart = Cart Int Int Facing Dir
  deriving Show

instance Eq Cart where
  Cart x y _ _ == Cart z w _ _ = (y,x) == (w,z)

instance Ord Cart where
  Cart x y _ _ `compare` Cart z w _ _ = (y,x) `compare` (w,z)

main :: IO ()
main = runMain 13 parser part1 part2

part1 :: Input -> (Int, (Int,Int))
part1 (t',c') = go t' c' 0 where
  go t c k = case allStep t c of
             Right c2 -> go t c2 (k+1)
             Left n -> (k, n)

part2 :: Input -> (Int,Int)
part2 (t',c') = go t' c' where
  go t c = case allStep' t c of
             Right c2 -> go t (traceShowId c2)
             Left n -> place n

allStep :: Map (Int,Int) Track -> [Cart] -> Either (Int,Int) [Cart]
allStep t cs = let stepped = map (oneStep t) cs
                in case check stepped of
                     Nothing -> Right (sort stepped)
                     Just t' -> Left t'

allStep' :: Map (Int,Int) Track -> [Cart] -> Either Cart [Cart]
allStep' t cs = let stepped = helper t cs
                in case removeCrash stepped of
                     [x] -> Left x
                     [] -> error "empty?"
                     t' -> Right (sort t')

helper :: Map (Int,Int) Track -> [Cart] -> [Cart]
helper _ [] = []
helper t (c:cs) = let c' = oneStep t c
                   in if c' `elem` cs
                         then helper t (c' `delete` cs)
                         else c': helper t cs

check :: [Cart] -> Maybe (Int,Int)
check = fmap place . run S.empty

removeCrash :: [Cart] -> [Cart]
removeCrash = S.toList . run' S.empty

run' :: Ord a => Set a -> [a] -> Set a
run' seen (x:xs)
  | x `S.member` seen = run' (S.delete x seen) xs
  | otherwise         = run' (S.insert x seen) xs
run' s [] = s

place :: Cart -> (Int,Int)
place (Cart x y _ _) = (x,y)

run :: Ord a => Set a -> [a] -> Maybe a
run seen (x:xs)
  | x `S.member` seen = Just x
  | otherwise         = run (S.insert x seen) xs
run _ [] = Nothing

oneStep :: Map (Int,Int) Track -> Cart -> Cart
oneStep ts (Cart x y f d) = let trackDir = ts ! (x,y)
                             in case (trackDir,f) of
                                  (Hori, L) -> Cart (x-1) y f d
                                  (Hori, R) -> Cart (x+1) y f d
                                  (Vert, U) -> Cart x (y-1) f d
                                  (Vert, D) -> Cart x (y+1) f d
                                  (Hori, _) -> undefined
                                  (Vert, _) -> undefined
                                  (For, R) -> Cart x (y-1) U d
                                  (For, D) -> Cart (x-1) y L d
                                  (For, U) -> Cart (x+1) y R d
                                  (For, L) -> Cart x (y+1) D d
                                  (Back, R) -> Cart x (y+1) D d
                                  (Back, D) -> Cart (x+1) y R d
                                  (Back, U) -> Cart (x-1) y L d
                                  (Back, L) -> Cart x (y-1) U d
                                  (Inter, _) -> case (d,f) of
                                                  (Lef,L) -> Cart x (y+1) D Straight
                                                  (Lef,D) -> Cart (x+1) y R Straight
                                                  (Lef,R) -> Cart x (y-1) U Straight
                                                  (Lef,U) -> Cart (x-1) y L Straight
                                                  (Straight, L) -> Cart (x-1) y L Righ
                                                  (Straight, D) -> Cart x (y+1) D Righ
                                                  (Straight, R) -> Cart (x+1) y R Righ
                                                  (Straight, U) -> Cart x (y-1) U Righ
                                                  (Righ,L) -> Cart x (y-1) U Lef
                                                  (Righ,D) -> Cart (x-1) y L Lef
                                                  (Righ,R) -> Cart x (y+1) D Lef
                                                  (Righ,U) -> Cart (x+1) y R Lef


parser :: String -> Input
parser xs = (ts, cs)
  where ts = M.fromList [((x,y),t) | (y,l) <- zip [0..] ls, (x,v) <- zip [0..] l, t <- toList $ toTrack v]
        cs = sort [Cart x y f Lef | (y,l) <- zip [0..] ls, (x,v) <- zip [0..] l, f <- toList $ toFacing v]
        ls = lines xs

toTrack :: Char -> Maybe Track
toTrack '-' = Just Hori
toTrack '|' = Just Vert
toTrack '+' = Just Inter
toTrack '\\' = Just Back
toTrack '/' = Just For
toTrack '>' = Just Hori
toTrack '<' = Just Hori
toTrack '^' = Just Vert
toTrack 'v' = Just Vert
toTrack ' ' = Nothing
toTrack _ = error "bad char"

toFacing :: Char -> Maybe Facing
toFacing '>' = Just R
toFacing '<' = Just L
toFacing '^' = Just U
toFacing 'v' = Just D
toFacing _ = Nothing
