--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------

-- | This module should contain your game code.
  module Game where
  import Data.List
  import Data.Ix
  --------------------------------------------------------------------------------

  -- | The number of pegs in a code.
  pegs :: Int
  pegs = 4

  -- | Symbols are represented by characters.
  type Symbol = Char

  -- | The available symbols in the game.
  symbols :: [Symbol]
  symbols = ['a'..'f']

  -- | A code is represented by a list of symbols.
  type Code = [Symbol]

  -- | Guesses are scored using coloured and white markers. The first component
  -- of the pair gives the number of coloured markers and the right component
  -- gives the number of white markers.
  type Score = (Int, Int)

  -- | A player is either human or computer-controlled.
  data Player = Human | Computer

  -- | The first codemaker in a play session.
  codemaker :: Player
  codemaker = Human

  -- | The first guess the AI will make.
  firstGuess :: Code
  firstGuess = "aabb"

  --------------------------------------------------------------------------------





  -- | Determines whether a score indicates that a guess was correct or not.
  ------------------------------------
  {-- A simple test that checks whether the score says all the peg have the right
      colour and position i.e. max (pegs) number of coloured markers and no white markers --}
  correctGuess:: Score -> Bool
  correctGuess s =  s == (pegs,0)





  -- TODO: Remove elemOf ?
  -- -- | Same as elem function but takes list first then the term
  -- -- Used so the higher order functions is neater
  -- elemOf :: (Foldable t, Eq a) => t a -> a -> Bool





  -- | This function should check that the code entered by a human player is
  -- valid. In other words, it should have the length given by `pegs` and it
  -- should only contain valid symbols.
  ------------------------------------
  {--
  If the code is the correct length and
  if all elements are also elements of symbols
  else its invalid
  --}
  validateCode :: Code -> Bool
  validateCode xs
    | length xs == pegs   = all (`elem` symbols) xs
    | otherwise           = False





    -- | All possible codes.
  ------------------------------------
  {--
  code = all possible permuations symbol that are length n of
  --}
  codes :: [Code]
  codes = permute pegs symbols

  {--
  TODO: Better explanations?
  The permuation set of length n (p n) is
  the elements in (p n-1) cons each possible element
  --}
  permute :: Int -> [a] -> [[a]]
  permute 0 _  = [[]]
  permute n xs = [ x:ys | x <- xs, ys <- permute (n-1) xs]





  -- | All possible scores.
  ------------------------------------
  {--
    Score all possible guesses (codes) against a
    code pulled from the list of symbols (c).
    nub removes all duplicates and leave the set of all possible results
    (cycle used incase number of symbols is less then num of pegs)
  --}
  results :: [Score]
  results = let c = take pegs (cycle symbols)
            in
              nub $ map (score c) codes





  -- | Scores a guess against a code. Symbols which are in the right place
  -- and of the right type score a coloured marker. Symbols which are of the
  -- right type but in the wrong place score a white marker.
  ------------------------------------
  -- [Your explanation]
  --TODO :: countColored countWhite COMBINE
  {--

  --}
  score :: Code -> Code -> Score
  score code guess = (c,w-c)
    where
      c = countColored code guess
      w = countWhite code guess



  countColored :: Code -> Code -> Int
  -- countColored xs ys = count elem xs ys
  countColored []      _    = 0
  countColored (x:xs) (y:ys)
    | x == y                = 1 + countColored xs ys
    | otherwise             =     countColored xs ys

  countWhite :: Code -> Code -> Int
  -- countWhite xs ys = count (==) xs ys
  countWhite []     _     = 0
  countWhite (x:xs) guess
    | x `elem` guess      = 1 + countWhite xs (delete x guess)
    | otherwise           =     countWhite xs guess


  -- count :: Eq a => ([a] -> [a] -> Bool) -> Code -> Code -> Int
  -- count _ [] _ = 0
  -- count p lx ly
  --     | p lx ly      = 1 + count xs (delete x ly)
  --     | otherwise    = count xs ly
  --       where
  --         (x:xs) = lx
  --         (y:ys) = ly





  -- | Chooses the next guess. If there is only one option left, choose it.
  -- Otherwise, calculate the hit score for each code and choose the code
  -- with the largest hit score.
  ------------------------------------
  -- [Your explanation]
  {--

  --}
  nextGuess :: [Code] -> Code
  nextGuess s = undefined





  -- | Remove all codes from the remaining possibilities which would result in
  -- a different score for the guess if they were the code.
  -- In other words, given the set of remaining possible codes, narrow it down
  -- to those which would produce the same score we got from the codemaker.
  ------------------------------------
  -- [Your explanation]

  --}
  eliminate :: Score -> Code -> [Code] -> [Code]
  eliminate lastScore guess codes = [ x | x <- codes, (score x guess) == lastScore]

  --------------------------------------------------------------------------------
