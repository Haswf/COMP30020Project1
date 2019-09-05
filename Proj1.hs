
module Proj1 (feedback, initialGuess, nextGuess, GameState) where
    
import Card
import Data.List
import Data.Set


type Feedback = (Int, Int, Int, Int, Int)

-- GameState is a type used to store game information across multiple guesses.
data GameState = GameState{
                        mismatchLeft :: [Card], -- cards that haven't been test for incorrect
                        matchLeft :: [Card], -- cards that haven't been test for corect
                        mismatchCard :: [Card], -- A set of cards which have been tested to be incorrect
                        matchCard :: Set Card   -- 
                        } deriving (Show)

-- Provide feedback on the answer and the guess
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

-- Create an inisital card based on a given numberOfCardsber of cards
initialGuess :: Int -> ([Card], GameState)
initialGuess numberOfCards = (guess, gamestate)
    where guess = take numberOfCards allCards -- take first N cards as first guess where N is the numberOfCardsber of cards to guess
          gamestate = GameState { mismatchLeft=allCards Data.List.\\ guess,
                                  matchLeft=allCards, 
                                  mismatchCard=[], 
                                  matchCard=empty
                                }
          -- all cards in card type
          allCards = [minBound..maxBound] :: [Card]

guessMismatchCard :: ([Card], GameState) -> Feedback -> ([Card], GameState)
guessMismatchCard (thisGuess, GameState mismatchLeft matchLeft mismatchCard matchCard)
          (exactMatch, _, _,_,_) =      -- at this stage, we don't care other feedback
        -- build new mismatchLeft
    let newmismatchLeft 
            | exactMatch == 0 = []
            | otherwise = tail mismatchLeft
        -- build new matchLeft
        newmatchLeft 
            -- if no card in this guess matches target, remove them from matchLeft
            | exactMatch == 0 = matchLeft Data.List.\\ thisGuess
            | otherwise = matchLeft
        -- build new mismatchCard
        newmismatchCard 
            -- if no card in this guess matches target, add them to mistmatchCard
            | exactMatch == 0 = thisGuess
            | otherwise = mismatchCard
        -- Construct new GameState
        newGameState = GameState {  mismatchLeft=newmismatchLeft,
                                    matchLeft=newmatchLeft,
                                    mismatchCard=newmismatchCard,
                                    matchCard=matchCard}
        -- append first card in mismatchLeft to form next guess
        newGuess = tail thisGuess ++ [head mismatchLeft]
    in (newGuess, newGameState)

-- Generate next guess based on GameState and Feedback of previous guess
nextGuess :: ([Card],GameState) -> Feedback -> ([Card],GameState)
nextGuess (thisGuess, thisGameState) fb
    -- if all cards in target were added to matchCard 
    | length (matchCard thisGameState) == answerLen = (toList (matchCard thisGameState), thisGameState)
    -- if we don't have enough mismatch ard in mismatchCard, keep finding mismatched cards.
    | length (mismatchCard thisGameState) /= answerLen = guessMismatchCard (thisGuess, thisGameState) fb
    -- otherwise, keep looking for matched cards.
    | otherwise = guessMatchCard (thisGuess, thisGameState) fb
    where answerLen = length thisGuess -- the length of target cards


guessMatchCard :: ([Card],GameState) -> Feedback -> ([Card],GameState)
guessMatchCard (thisGuess, GameState mismatchLeft matchLeft mismatchCard matchCard) (exactMatch, smaller, equal,greater,matchSuit) = 
    let newmatchCard = case exactMatch of 0 -> matchCard
                                          1 -> Data.Set.insert (last thisGuess) matchCard

        newmatchLeft
            | length newmatchCard == ansLength = toList matchCard
            | otherwise = extractmatchLeft (filterOutSuit $ filterOutGreater $ filterOutSmaller (thisGuess, matchLeft, (exactMatch, smaller, equal ,greater,matchSuit))) 

        newGuess = twoWrongCards ++ [head newmatchLeft]

        newGameState = GameState {  mismatchLeft=[],
                                    matchLeft=tail newmatchLeft, -- no need to change right left
                                    mismatchCard=mismatchCard,
                                    matchCard=newmatchCard}

    in (newGuess, newGameState)
    where ansLength = length thisGuess
          twoWrongCards = take (ansLength-1) mismatchCard

-- Extract newmatchLeft from intermediate tuple for filters.
extractmatchLeft :: ([Card],[Card], Feedback) -> [Card]
extractmatchLeft (_, newmatchLeft, _) = newmatchLeft

-- filter out cards in matchLeft which have greater rank than maximum card in this guess.
filterOutGreater ::  ([Card],[Card], Feedback) -> ([Card],[Card], Feedback) 
filterOutGreater (thisGuess, matchLeft, (exactMatch, smaller, equal,greater,matchSuit)) = 
    -- Extract ranks of all cards to a list
    let guessRank = Data.List.map getRank thisGuess
        -- Find the Highest rank in the guess
        highestRank = maximum guessRank
        newmatchLeft 
            | greater == 0 = Data.List.foldr (\card acc-> if getRank card >highestRank then acc else card:acc) [] matchLeft 
            | otherwise = matchLeft
        in (thisGuess, newmatchLeft, (exactMatch, smaller, equal,greater,matchSuit))

-- filter out cards in matchLeft which have smaller rank than minimum card in this guess.
filterOutSmaller ::  ([Card],[Card], Feedback) -> ([Card],[Card], Feedback) 
filterOutSmaller (thisGuess, matchLeft, (exactMatch, smaller, equal,greater,matchSuit)) = 
        -- Extract ranks of all cards to a list
    let guessRank = Data.List.map getRank thisGuess
        -- Find the lowest rank in the guess
        lowestRank = minimum guessRank
        -- Build new matchLeft
        newmatchLeft 
            -- No card in guess has lower tank than the lowest rank in the guess, filter out cards with enen lower rank in matchLeft.
            | smaller == 0 = Data.List.foldr (\card acc-> if getRank card <lowestRank then acc else card:acc) [] matchLeft
            -- Otherwise, keep matchLeft as it is.
            | otherwise = matchLeft
        in (thisGuess, newmatchLeft, (exactMatch, smaller, equal,greater,matchSuit))

filterOutSuit :: ([Card],[Card], Feedback) -> ([Card],[Card], Feedback) 
filterOutSuit (thisGuess, matchLeft, (exactMatch, smaller, equal,greater,matchSuit))  = 
    let newmatchLeft
            -- if no card in the current guess has same suit as targets, filter out cards in matchLeft with same suit.
            | matchSuit == 0 = Data.List.foldr (\card acc -> if getSuit card `elem` suits then acc else card:acc) [] matchLeft
            -- if all cards in the current guess has same suit as targets, filter out cards in matchLeft with different suit.
            | matchSuit == length thisGuess = Data.List.foldr (\card acc -> if getSuit card `elem` suits then card:acc else acc) [] matchLeft
            -- Otherwise, keep matchLeft as it is.
            | otherwise = matchLeft
            -- Extract suits of all cards to a list
            where suits = Data.List.map getSuit thisGuess 

        in (thisGuess, newmatchLeft,  (exactMatch, smaller, equal,greater,matchSuit))