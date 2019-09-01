
module Proj1 (feedback, initialGuess, nextGuess, GameState) where
    
import Card
import Data.List

type Feedback = (Int, Int, Int, Int, Int)

data GameState = GameState{
                        wrongLeft :: [Card],
                        rightLeft :: [Card],
                        wrongGuess :: [Card],
                        correctGuess :: [Card]
                        } deriving (Show)

feedback :: [Card] -> [Card] -> Feedback
feedback answer guess = (length exactMatch,
                         length lowerRank,
                         length equalRank,
                         length greaterRank,
                         length equalSuit)
    where exactMatch = answer `intersect` guess
          guessRank = map getRank guess
          answerRank = map getRank answer
          lowestRank = minimum guessRank
          highestRank = maximum guessRank
          lowerRank = filter (<lowestRank) answerRank
          equalRank = answerRank `intersect` guessRank
          greaterRank = filter (>highestRank) answerRank
          equalSuit =  map getSuit guess `intersect` map getSuit answer

getRank :: Card -> Rank
getRank (Card _ r) = r

getSuit :: Card -> Suit
getSuit (Card s _) = s

-- Parse guess from input
getGuess :: ([Card], GameState) -> [Card]
getGuess (guess, _) = guess

-- Parse GameState from input
getGameState :: ([Card], GameState) -> GameState
getGameState (_, gs) = gs

initialGuess :: Int -> ([Card], GameState)
initialGuess num = (guess, gamestate)
    where guess = take num allCards
          gamestate = GameState { wrongLeft=allCards \\ guess, 
                                  rightLeft=allCards, 
                                  wrongGuess=[], 
                                  correctGuess=[]}
          -- all cards in card type
          allCards = [minBound..maxBound] :: [Card]

findWrong :: ([Card], GameState) -> Feedback -> ([Card], GameState)
findWrong (thisGuess, GameState wrongLeft rightleft wrongguess correctguess) (exactMatch, _, _,_,_)
    | exactMatch == 0 = let nextStageGameState =  GameState { wrongLeft=[], -- found enough wrong cards, no longer need to keep cards haven't been tested to be wrong
                                                              rightLeft=rightleft \\ thisGuess, -- no need to change right left
                                                              wrongGuess=thisGuess, -- store wrong cards in wrong guess
                                                              correctGuess=[]}
                        in (thisGuess, nextStageGameState)
    | otherwise = let newGuess = tail thisGuess ++ [head wrongLeft]  -- generate next guess to find wrong cards
                      newGameState = GameState { wrongLeft=tail wrongLeft,
                                                 rightLeft=rightleft, -- no need to change right left
                                                 wrongGuess=wrongguess,
                                                 correctGuess=correctguess}
                     in (newGuess, newGameState)


nextGuess :: ([Card],GameState) -> Feedback -> ([Card],GameState)
nextGuess (thisGuess, thisGameState) fb
    -- if we haven't found length card distinct cards that's not in target
    | length (correctGuess thisGameState) == answerLen = ((correctGuess thisGameState), thisGameState)
    | length (wrongGuess thisGameState) /= answerLen = findWrong (thisGuess, thisGameState) fb
    | otherwise = findCorrect (thisGuess, thisGameState) fb
    where answerLen = length thisGuess
          

findCorrect :: ([Card],GameState) -> Feedback -> ([Card],GameState)
findCorrect (thisGuess, GameState wrongLeft rightleft wrongguess correctguess) (exactMatch, _, _,_,_)
    | exactMatch == 0 = let newGuess = twoWrongCards ++ [head rightleft]  -- generate next guess to find wrong cards
                            newGameState = GameState { wrongLeft=[],
                                                       rightLeft=tail rightleft, -- no need to change right left
                                                       wrongGuess=wrongguess,
                                                      correctGuess=correctguess}
                        in (newGuess, newGameState)
    | otherwise = let newGuess = twoWrongCards ++ [head rightleft]  -- generate next guess to find wrong cards
                      newGameState = GameState { wrongLeft=[],
                               rightLeft=tail rightleft, -- no need to change right left
                               wrongGuess=wrongguess,
                               correctGuess=correctguess++[last thisGuess]}
                        in (newGuess, newGameState)
    where ansLength = length thisGuess
          twoWrongCards = take (ansLength-1) wrongguess


-- getWrong :: GameState -> [Card]
-- getWrong (GameState _  wrong  _) = wrong

-- getCorrect :: GameState -> [Card]
-- getCorrect (GameState _  _  correct) = correct

-- getLeft :: GameState -> [Card]
-- getLeft (GameState left  _  _) = left
