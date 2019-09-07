-- Solution to COMP30020 Project 1 2019S2
-- @author Shuyang Fan shuyangf(AT)student.unimelb.edu.au
 
module Proj1 (feedback, initialGuess, nextGuess, GameState) where
    
import Card
import Data.List

-- | Feedback is a synonym for five Ints so no need to write long tuple again.
type Feedback = (Int, Int, Int, Int, Int)

-- | Type GameState is defined as a list of lists of cards representing 
--   the remaining possible card combiantions.
type GameState = [[Card]]

-- | Provides five numbers feedback on how the guess differs/similars from the answer, which are
--   correct cards, lower ranks, correct ranks, higher ranks and correct suits.
--   It takes a list cards as answer and a list of cards as guess, returns a Feedback as define above.
feedback :: [Card] -> [Card] -> Feedback
feedback answer guess = (length correctCards,
                         length lowerRank,
                         length sameRank,
                         length higherRank,
                         length sameSuit)
    where correctCards = answer `intersect` guess
          -- Ranks of cards in guess.
          guessRank = map getRank guess
          -- Ranks of cards in answer.
          answerRank = map getRank answer
          -- Filter the cards in the answer that has a lower rank than the card with the lowest rank in guess.          
          lowerRank = let lowestRank = minimum guessRank 
                        in filter (<lowestRank) answerRank
        
          -- Filter out the cards in the answer that has the same rank as a card in the guess.          
          sameRank = answerRank `intersect` guessRank

          -- Filter out the cards in the answer that has a higher rank than the card with the lowest rank in guess.          
          higherRank = let highestRank = maximum guessRank 
                            in filter (>highestRank) answerRank
          -- Suits that appear the same time in answer and guess.
          sameSuit = map getSuit answer `intersect` map getSuit guess

-- | Helper function to extract the rank of a card.
--   getRank takes a Card type and extract its rank by pattern matching.
getRank :: Card -> Rank
getRank (Card _ r) = r

-- | Helper function to extract the suit of a card.
--   getSuit takes a Card type and extract its suit by pattern matching.
getSuit :: Card -> Suit
getSuit (Card s _) = s

-- | Create initial Guess of type ([Card],GameState) with a given Int
--   representing number of cards in answer.
initialGuess :: Int -> ([Card],GameState)
initialGuess guessLen = (goodStart, allCombinations)
        -- Generate initial guess based on the number of cards. See generateInitialGuess.
    where goodStart = generateInitialGuess guessLen
        -- All cards in a deck.
          allCards = [minBound..maxBound] :: [Card]
        -- Combinations is called to generation all combination of length guessLen.
          allCombinations = combinations guessLen allCards

-- | Return middle element of a list. 
--   In case of list has even length, the later element is extracted.
getMiddle :: [a] -> a
getMiddle lst = lst !! midIndex
            where midIndex = length lst `div` 2

            
-- | Generate initial guess by choosing n cards of different suits 
--   and with ranks about equally distant from each other and from 
--   the top and bottom ranks. This was achieved by chunk rank into
--   13 / guessLen sections and extract the middle element of each trunk.
generateInitialGuess :: Int -> [Card]
generateInitialGuess guessLen = let suits = take guessLen [minBound..maxBound] :: [Suit]
                                    -- number of chunks
                                    chunks = 13 `div` guessLen
                                    -- Get middle element of each chunk
                                    ranks = map getMiddle $ chunksOf chunks [minBound..maxBound] :: [Rank]
                            -- Create cards using Card constructor
                            in zipWith Card suits ranks 

-- | Split a list into chunks of size n. 
-- | It takes a Int representing chunk size and a list to be splited as arguments.
-- | The list will be returned as it is if it has less elements than N.
chunksOf :: Int -> [a] -> [[a]]
chunksOf chunkLength lst
  | length lst >= chunkLength = take chunkLength lst: chunksOf chunkLength rest
  | otherwise = [lst]
  where rest = drop chunkLength lst


                            

-- | Generates the combinations of K distinct objects chosen from the N elements of a list.
--   It takes an Int indicating the number of elements each combination will have and a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n lst = [y:ys | y:rest <- tails lst, 
                               ys <- combinations (n-1) rest]

-- | Return a new ([Card], GameState) pair representing next guess and new GameState 
--   based on the feedback on previous guess. It takes a ([Card], GameState) pair 
--   representing previous guess and GameState and Feedback on the previous guess.
nextGuess :: ([Card],GameState) -> Feedback -> ([Card],GameState)
nextGuess (guess, gameState) fb = (head newGameState, newGameState)
    -- Narrows down possible answers by only keeping card combiantions 
    -- such that have the same feedback when comparing to the previous guess.
    where newGameState = [x | x <- gameState, feedback x guess == fb]