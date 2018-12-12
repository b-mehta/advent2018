import Common
import Parsers
import qualified Data.MultiSet as MS

type Input = [(Name, Coords)]
type Coords = ((Int,Int),(Int,Int))
type Name = Int

type Ground = MS.MultiSet (Int,Int)

main :: IO ()
main = runMainP 3 parser part1 part2

part1 :: Input -> Int
part1 = MS.distinctSize . overlayClaims

overlayClaims :: Input -> Ground
overlayClaims = msOccFilter (>=2) . MS.unions . map (claim . snd)

claim :: Coords -> Ground
claim ((x,y),(w,h)) = MS.fromList [(a,b) | a <- [x..x+w-1], b <- [y..y+h-1]]

part2 :: Input -> Name
part2 s = head (mapMaybe (emptyPlace badSpots) s)
  where badSpots = overlayClaims s

emptyPlace :: Ground -> (Name, Coords) -> Maybe Name
emptyPlace g (r, c) = if MS.null (MS.intersection (claim c) g)
                         then Just r
                         else Nothing

parser :: Parser Input
parser = lineParser line

line :: Parser (Name, Coords)
line = char '#' *> middle num " @ " coords

coords :: Parser Coords
coords = middle magicNums ": " magicNums
