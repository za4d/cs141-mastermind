--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------


-- | This module should contain your game code.
module Game where
  import Data.List
  import Data.Ix
  import Data.Ord
  --
  import Control.Arrow ((***),second,first)
  import Data.Function (on)
  import System.IO
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
  -- | Simply checks whether all the score pegs are coloured and
  --------------------------------------------------------------------------------
  correctGuess:: Score -> Bool
  correctGuess s =  s == (pegs,0)



  ------------------------------------
  -- | This function checks the code is valid by testing...
  --    - whether it's the correct length
  --    - if so, are all of its elements in 'symbols'
  ------------------------------------
  validateCode :: Code -> Bool
  validateCode xs
    | length xs == pegs   = all (`elem` symbols) xs
    | otherwise           = False



  ------------------------------------
  -- | All possible codes found recursivly.
  -- All possible permuations of length n =
  -- All symbols : All possible permuations of length n-1
  ------------------------------------
  codes :: [Code]
  codes = permutate pegs symbols

  permutate :: Int -> [a] -> [[a]]
  permutate 0 _  = [[]]
  permutate n xs = [ x:ys | x <- xs, ys <- permutate (n-1) xs]



  ------------------------------------
  -- | All possible scores genererated by scoring all possible guesses (codes)
  -- against a code (c) pulled from the list of possible symbols. 'nub'
  -- then removes all duplicates and leave the set of all possible unique results
  -- (cycle used incase number of symbols is less then num of pegs)
  ------------------------------------
  results :: [Score]
  results = let
              c = take pegs (cycle symbols)
            in
              nub $ map (score c) codes





  --TODO :: countColored countWhite COMBINE
  ------------------------------------
  -- |
  ------------------------------------
  score' :: Code -> Code -> Score
  score' code guess = answer code guess

  answer :: Code -> Code -> Score
  answer xs = (length *** s) . partition (uncurry (==)) . zip xs
     where
     f  x (n,ys)  | x `elem` ys = (n + 1, delete x ys)
                  | otherwise = (n,ys)
     s xys = fst $ foldr f (0,ys') xs' where
       (xs',ys') = unzip xys


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




  -----------------------------------
  -- | Instead of finding the maximin of codes removed from S, it was easier
  -- to use the 'eliminate' function and find the contrapositive equivilent
  -- i.e. the minimax of codes left in S. So the function finds the guess
  -- in S with the shortest: longest possible S after elimination (max_s)
  ------------------------------------

  nextGuess :: [Code] -> Code
  nextGuess (ans:[]) = ans
  nextGuess s =
    nextGuessM s -- MAIN
    -- nextGuessT s -- TEST

-- "acbf" "acfc"
-----
  nextGuessT :: [Code] -> Code
  nextGuessT s =  if (length s) == 1
                  then
                    head s
                  else
                    minimumBy (comparing valg) codes
                      where valg g =
                                      maximum . map length . groupOn id . map (score g) $ s

  groupOn :: Ord b => (a -> b)  -> [a] -> [[a]]
  groupOn f = groupBy ((==) `on`  f) . sortBy (comparing f)
  {--
  (+) `on` f = \x y -> f x + f y
  groupBy [a]->[[]] according to condition
  --}
-----
  nextGuessM :: [Code] -> Code
  nextGuessM s = maximumBy (comparing go) ((codes) ++ s)
                  where go g = minimum [length (removals r g s) | r <- results]

                 -- minimumBy (comparing max_s) s
                 --    where max_s g = maximumBy (comparing length) [eliminate r g s | r <- results]
           -- returns the largest S a 'g'uess could leave

  temp :: [Code]
  temp =
    -- eliminate (2,0) "abbb" codes
    ["abcd","abbb","abda","abdc","abdd","abde","aaaa","ffff","cccc"]

  tt s = minimumBy (comparing (tmax s)) codes

  t6 = minimum t5
  t5 = map (tmax temp) codes
  --
  tmax s g = maximum . map length . groupOn id . map (score g) $ s
  t4 = maximum t3 -- return largest length
  t3 = map length t2 -- seperate the scores into lengths
  t2 = groupBy (==) . sort $ t1 -- sort and group equivilent scores
  t2' = groupOn id t1 --
  t1 = (\g -> map (score g) t0 ) $ "abbb" -- score each in S according to the guess
  t0 = temp -- possible codes

  r0 = [removals r "abbb" temp | r <- results]
  r1 = map length r0
  r2 = minimumBy (comparing length) [ removals r "abbb" temp | r <- results]

  rr = maximumBy (comparing go) ((temp \\ codes) ++ temp)
  go g = minimum [length (removals r g temp) | r <- results]

  rrr = filter (\x -> go x == go rr) codes

  rr1 g = eliminate (score "abbb" g) g temp
  rr2 g = removals (score "abbb" g) g temp
-----
  removals :: Score -> Code -> [Code] -> [Code]
  removals lastScore guess codes = codes \\ (eliminate lastScore guess codes)

  removals' :: Score -> Code -> [Code] -> Int
  removals' lastScore guess codes = length $ codes \\ (eliminate lastScore guess codes)

  ------------------------------------
  -- |
  ------------------------------------
  eliminate :: Score -> Code -> [Code] -> [Code]
  eliminate lastScore guess codes = [ c | c <- codes, (score c guess) == lastScore]

  --------------------------------------------------------------------------------
