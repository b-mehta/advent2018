import Common

type Input = [Record]
type Record = Void

type Parser a = Parsec Void String a

type Part1 = Void
type Part2 = Void

main :: IO ()
main = runMainP n parser part1 part2

part1 :: Input -> Part1
part1 = error "todo: part 1"

part2 :: Input -> Part2
part2 = error "todo: part 2"

parser :: Parser Input
parser = error "todo: parser"
