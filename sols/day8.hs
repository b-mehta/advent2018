import Common
import Control.Monad.State

data Tree = Tree [Tree] [Int]
  deriving Show

main :: IO ()
main = runMain 8 parser sumMetadata scoreTree

sumMetadata :: Tree -> Int
sumMetadata (Tree cs ms) = sum (map sumMetadata cs ++ ms)

scoreTree :: Tree -> Int
scoreTree (Tree [] ms) = sum ms
scoreTree (Tree cs ms) = sum . map (childScores !!) . filter (< len) . map (subtract 1) $ ms
  where childScores = map scoreTree cs
        len = length childScores

readTree :: StateT [Int] Maybe Tree
readTree = do
  (children, metadata) <- (,) <$> readNum <*> readNum
  Tree <$> replicateM children readTree <*> replicateM metadata readNum
    where readNum = StateT uncons

makeTree :: [Int] -> Tree
makeTree = fromMaybe (error "bad tree") . evalStateT readTree

parser :: String -> Tree
parser = makeTree . map read . words
