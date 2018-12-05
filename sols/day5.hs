import Common

main :: IO ()
main = runMain 5 init part1 (part2 . fullReact)

part1 :: String -> Int
part1 = length . fullReact

fullReact :: String -> String
fullReact = foldr match []

match :: Char -> String -> String
match y t
  | x:stk <- t, cancel x y = stk
  | otherwise              = y:t

cancel :: Char -> Char -> Bool
cancel x y = toLower x == toLower y && x /= y

part2 :: String -> Int
part2 s = minimum [part1 $ remove x s | x <- ['a'..'z']]

remove :: Char -> String -> String
remove c = filter ((/= c) . toLower)
