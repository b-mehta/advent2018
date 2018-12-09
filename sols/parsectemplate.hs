{-# LANGUAGE PartialTypeSignatures #-} -- temp
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-} -- temp

import Common

main :: IO ()
main = runMainP n parser part1 part2

part1 :: _ -> _
part1 = error "todo: part 1"

part2 :: _ -> _
part2 = error "todo: part 2"

parser :: Parser _
parser = error "todo: parser"

-- parser :: Parser [_]
-- parser = lineParser line
--
-- line :: Parser _
-- line = error "todo: line parser"
