
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
          equalSuit =  Data.List.map getSuit answer `intersect` Data.List.map getSuit guess

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

extractRightLeft :: ([Card],[Card], Feedback) -> [Card]
extractRightLeft (_, newrightleft, _) = newrightleft

findCorrect :: ([Card],GameState) -> Feedback -> ([Card],GameState)
findCorrect (thisGuess, GameState wrongLeft rightleft wrongguess correctguess) (exactMatch, smaller, equal,greater,matchSuit) = 
    let guessRank = Data.List.map getRank thisGuess
        lowestRank = minimum guessRank
        highestRank = maximum guessRank
        newCorrectGuess = case exactMatch of 0 -> correctguess
                                             1 -> Data.Set.insert (last thisGuess) correctguess

        newrightleft
            | length newCorrectGuess == ansLength = toList correctguess
            | otherwise = extractRightLeft (filterOutSuit $ filterOutGreater $ filterOutSmaller (thisGuess, rightleft, (exactMatch, smaller, equal ,greater,matchSuit))) 

        newGuess = twoWrongCards ++ [head newrightleft]

        newGameState = GameState {  wrongLeft=[],
                                    rightLeft=tail newrightleft, -- no need to change right left
                                    wrongGuess=wrongguess,
                                    correctGuess=newCorrectGuess}

    in (newGuess, newGameState)
    where ansLength = length thisGuess
          twoWrongCards = take (ansLength-1) wrongguess

-- filter out card in Rightleft which is greater than maximum card in this guess.
filterOutGreater ::  ([Card],[Card], Feedback) -> ([Card],[Card], Feedback) 
filterOutGreater (thisGuess, rightleft, (exactMatch, smaller, equal,greater,matchSuit)) = 
    let guessRank = Data.List.map getRank thisGuess
        highestRank = maximum guessRank
        newrightleft 
            | greater == 0 = Data.List.foldr (\card acc-> if getRank card >highestRank then acc else card:acc) [] rightleft 
            | otherwise = rightleft
        in (thisGuess, newrightleft, (exactMatch, smaller, equal,greater,matchSuit))

filterOutSmaller ::  ([Card],[Card], Feedback) -> ([Card],[Card], Feedback) 
filterOutSmaller (thisGuess, rightleft, (exactMatch, smaller, equal,greater,matchSuit)) = 
    let guessRank = Data.List.map getRank thisGuess
        lowestRank = minimum guessRank
        newrightleft 
            | smaller == 0 = Data.List.foldr (\card acc-> if getRank card <lowestRank then acc else card:acc) [] rightleft
            | otherwise = rightleft
        in (thisGuess, newrightleft, (exactMatch, smaller, equal,greater,matchSuit))

filterOutSuit :: ([Card],[Card], Feedback) -> ([Card],[Card], Feedback) 
filterOutSuit (thisGuess, rightleft, (exactMatch, smaller, equal,greater,matchSuit))  = 
    let newrightleft
            -- if no card in current guess has same suit as targets, filter out cards with same suit as this guess
            | matchSuit == 0 = Data.List.foldr (\card acc -> if getSuit card `elem` thisSuit then acc else card:acc) [] rightleft
            | matchSuit == length thisGuess = Data.List.foldr (\card acc -> if getSuit card `elem` thisSuit then card:acc else acc) [] rightleft
            -- -- if majority of this guess have the right suit, 
            -- | let halfLengt = matchSuit > div (length thisGuess) 2 = 
            --     Data.List.foldr (\card acc -> if getSuit card == listMode thisSuit then card:acc else acc) [] rightleft
            | otherwise = rightleft
            where thisSuit = Data.List.map getSuit thisGuess 

        in (thisGuess, newrightleft,  (exactMatch, smaller, equal,greater,matchSuit))

listMode :: Ord a => [a] -> a
listMode = snd . maximum . Data.List.map (\xs -> (length xs, head xs)) . group . sort