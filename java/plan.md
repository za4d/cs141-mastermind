1. input code
2. guess score guess score done.

1.

# Maps
symbols = ['a'..'f']
type Code = [Symbol]
type Score = (Int, Int)
codes = permutate pegs symbols
results :: [Score]
data Player = Human | Computer
codemaker = Human

firstGuess = "aabb"

correctGuess s =  s == (pegs,0)
validateCode xs
validateCode :: Code -> Bool
score :: Code -> Code -> Score
countColored :: Code -> Code -> Int
countWhite :: Code -> Code -> Int
nextGuess :: [Code] -> Code
nextGuess s = mini max_s
eliminate :: Score -> Code -> [Code] -> [Code]
eliminate lastScore guess codes = [ c | c <- codes, (score c guess) == lastScore]
