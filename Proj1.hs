
module Proj1 (feedback, initialGuess, nextGuess, GameState) where
    
import Card
import Data.List
import Data.Set

type Feedback = (Int, Int, Int, Int, Int)

data GameState = GameState{
                        wrongLeft :: [Card], -- cards that haven't been test for incorrect
                        rightLeft :: [Card], -- cards that haven't been test for corect
                        wrongGuess :: [Card], -- A set of cards which have been tested to be incorrect
                        correctGuess :: Set Card
                        } deriving (Show)

feedback :: [Card] -> [Card] -> Feedback
feedback answer guess = (length exactMatch,
                         length lowerRank,
                         length equalRank,
                         length greaterRank,
                         length equalSuit)
    where exactMatch = answer `intersect` guess
          guessRank = Data.List.map getRank guess
          answerRank = Data.List.map getRank answer
          lowestRank = minimum guessRank
          highestRank = maximum guessRank
          lowerRank = Data.List.filter (<lowestRank) answerRank
          equalRank = answerRank `intersect` guessRank
          greaterRank = Data.List.filter (>highestRank) answerRank
          equalSuit =  Data.List.map getSuit guess `intersect` Data.List.map getSuit answer

-- helper function to extract rank of a card
getRank :: Card -> Rank
getRank (Card _ r) = r

-- helper function to extract suit of a card
getSuit :: Card -> Suit
getSuit (Card s _) = s

initialGuess :: Int -> ([Card], GameState)
initialGuess num = (guess, gamestate)
    where guess = take num allCards
          gamestate = GameState { wrongLeft=allCards Data.List.\\ guess, 
                                  rightLeft=allCards, 
                                  wrongGuess=[], 
                                  correctGuess=empty}
          -- all cards in card type
          allCards = [minBound..maxBound] :: [Card]

findWrong :: ([Card], GameState) -> Feedback -> ([Card], GameState)
findWrong (thisGuess, GameState wrongLeft rightleft wrongguess correctguess) (exactMatch, _, _,_,_) = 
    let newWrongLeft 
            | exactMatch == 0 = []
            | otherwise = tail wrongLeft
        newRightLeft 
            | exactMatch == 0 = rightleft Data.List.\\ thisGuess
            | otherwise = rightleft
        newWrongGuess 
            | exactMatch == 0 = thisGuess
            | otherwise = wrongguess
        newGameState = GameState {  wrongLeft=newWrongLeft, -- found enough wrong cards, no longer need to keep cards haven't been tested to be wrong
                                    rightLeft=newRightLeft, -- no need to change right left
                                    wrongGuess=newWrongGuess, -- store wrong cards in wrong guess
                                    correctGuess=empty}
        newGuess = tail thisGuess ++ [head wrongLeft]
    in (newGuess, newGameState)
        
nextGuess :: ([Card],GameState) -> Feedback -> ([Card],GameState)
nextGuess (thisGuess, thisGameState) fb
    -- if we haven't found length card distinct cards that's not in target
    | length (correctGuess thisGameState) == answerLen = (toList (correctGuess thisGameState), thisGameState)
    | length (wrongGuess thisGameState) /= answerLen = findWrong (thisGuess, thisGameState) fb
    | otherwise = findCorrect (thisGuess, thisGameState) fb
    where answerLen = length thisGuess


findCorrect :: ([Card],GameState) -> Feedback -> ([Card],GameState)
findCorrect (thisGuess, GameState wrongLeft rightleft wrongguess correctguess) (exactMatch, smaller, _,greater,_) = 
    let guessRank = Data.List.map getRank thisGuess
        lowestRank = minimum guessRank
        highestRank = maximum guessRank
        newCorrectGuess = case exactMatch of 0 -> correctguess
                                             1 -> Data.Set.insert (last thisGuess) correctguess

        newrightleft
            | length newCorrectGuess == ansLength = toList correctguess
            | smaller == 0 = Data.List.foldr (\card acc-> if getRank card <lowestRank then acc else card:acc) [] rightleft 
            | greater == 0 = Data.List.foldr (\card acc-> if getRank card >highestRank then acc else card:acc) [] rightleft 
            | otherwise = rightleft

        newGuess = twoWrongCards ++ [head newrightleft]

        newGameState = GameState {  wrongLeft=[],
                                    rightLeft=tail newrightleft, -- no need to change right left
                                    wrongGuess=wrongguess,
                                    correctGuess=newCorrectGuess}

    in (newGuess, newGameState)
    where ansLength = length thisGuess
          twoWrongCards = take (ansLength-1) wrongguess