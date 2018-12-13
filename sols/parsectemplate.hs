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
