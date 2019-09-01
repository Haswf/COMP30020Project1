
module Proj1 (feedback, initialGuess, nextGuess, GameState) where
    
import Card
import Data.List

type Feedback = (Int, Int, Int, Int, Int)

data GameState = GameState{
                        wrongLeft :: [Card], -- cards that haven't been test for incorrect
                        rightLeft :: [Card], -- cards that haven't been test for corect
                        wrongGuess :: [Card], -- A set of cards which have been tested to be incorrect
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
findWrong (thisGuess, GameState wrongLeft rightleft wrongguess correctguess) (exactMatch, _, _,_,_) = 
    let newWrongLeft 
            | exactMatch == 0 = []
            | otherwise = tail wrongLeft
        newRightLeft 
            | exactMatch == 0 = rightleft \\ thisGuess
            | otherwise = rightleft
        newWrongGuess 
            | exactMatch == 0 = thisGuess
            | otherwise = wrongguess
        newGameState = GameState {  wrongLeft=newWrongLeft, -- found enough wrong cards, no longer need to keep cards haven't been tested to be wrong
                                    rightLeft=newRightLeft, -- no need to change right left
                                    wrongGuess=newWrongGuess, -- store wrong cards in wrong guess
                                    correctGuess=[]}
        newGuess = tail thisGuess ++ [head wrongLeft]
    in (newGuess, newGameState)
        
nextGuess :: ([Card],GameState) -> Feedback -> ([Card],GameState)
nextGuess (thisGuess, thisGameState) fb
    -- if we haven't found length card distinct cards that's not in target
    | length (correctGuess thisGameState) == answerLen = ((correctGuess thisGameState), thisGameState)
    | length (wrongGuess thisGameState) /= answerLen = findWrong (thisGuess, thisGameState) fb
    | otherwise = findCorrect (thisGuess, thisGameState) fb
    where answerLen = length thisGuess
          
findCorrect :: ([Card],GameState) -> Feedback -> ([Card],GameState)
findCorrect (thisGuess, GameState wrongLeft rightleft wrongguess correctguess) (exactMatch, _, _,_,_) = 
    let newGuess = twoWrongCards ++ [head rightleft]
        newCorrectGuess = case exactMatch of 0 -> correctguess
                                             1 -> correctguess++[last thisGuess]
        newGameState = GameState {  wrongLeft=[],
                                    rightLeft=tail rightleft, -- no need to change right left
                                    wrongGuess=wrongguess,
                                    correctGuess=newCorrectGuess}

    in (newGuess, newGameState)
    where ansLength = length thisGuess
          twoWrongCards = take (ansLength-1) wrongguess