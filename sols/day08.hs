import Common
import Control.Monad.State

data Tree = Tree [Tree] [Int]

main :: IO ()
main = runMain 8 parser sumMetadata scoreTree

sumMetadata :: Tree -> Int
sumMetadata (Tree cs ms) = sum (map sumMetadata cs ++ ms)

scoreTree :: Tree -> Int
scoreTree (Tree [] ms) = sum ms
scoreTree (Tree cs ms) = sum (mapMaybe (`index` childScores) ms)
  where childScores = scoreTree <$> cs

index :: Int -> [a] -> Maybe a
index n = listToMaybe . drop (n-1)

parser :: String -> Tree
parser = fromMaybe (error "bad tree") . evalStateT readTree . map read . words

readTree :: StateT [Int] Maybe Tree
readTree = do (c, m) <- (,) <$> readNum <*> readNum
              Tree <$> replicateM c readTree <*> replicateM m readNum
           where readNum = StateT uncons
