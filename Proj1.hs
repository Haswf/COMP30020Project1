import Card
import Data.List

type Feedback = (Int, Int, Int, Int, Int)

data GameState = GameState{
                        remaining :: [Card],
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

getGuess :: ([Card], GameState) -> [Card]
getGuess (guess, _) = guess

getGameState :: ([Card], GameState) -> GameState
getGameState (_, gs) = gs

initialGuess :: Int -> ([Card], GameState)
initialGuess num = (guess, gamestate)
    where guess = take num allCards
          gamestate = GameState { remaining=allCards \\ guess, wrongGuess=[], correctGuess=[]}
          allCards = [minBound..maxBound] :: [Card]

findWrong :: ([Card], GameState) -> Feedback -> ([Card], GameState)
findWrong (thisGuess, GameState left wrong correct) (exactMatch, _, _,_,_)
    | exactMatch == 0 = (thisGuess,  GameState { remaining=left,
                                                 wrongGuess=thisGuess,
                                                 correctGuess=[]})
    | otherwise = (newGuess, newGameState)
    where newGuess = tail thisGuess ++ [head left]
          newGameState = GameState { remaining=tail left,
                                     wrongGuess=wrong,
                                     correctGuess=correct}
          

testFindWrong :: [Card] -> ([Card], GameState)
testFindWrong answer = findWrong (guess, gamestate) (feedback answer guess)
    where retval = initialGuess (length answer)
          guess = getGuess retval
          gamestate = getGameState retval

nextGuess :: ([Card],GameState) -> Feedback -> ([Card],GameState)
nextGuess (thisGuess, thisGameState) fb
    | length (wrongGuess thisGameState) /= answerLen = findWrong (thisGuess, thisGameState) fb
    | otherwise = initialGuess 3
    where answerLen = length thisGuess

getWrong :: GameState -> [Card]
getWrong (GameState _  wrong  _) = wrong

getCorrect :: GameState -> [Card]
getCorrect (GameState _  _  correct) = correct

getLeft :: GameState -> [Card]
getLeft (GameState left  _  _) = left

-- popNext :: GameState -> (Card, GameState)
-- popNext (GameState left wrongguess correctguess) = (nextCard, newGameState)
--     where nextCard = head left
--           newGameState = GameState { remaining=delete nextCard left,
--                                     wrongGuess=wrongguess,
--                                     correctGuess=correctguess}


----------------------------------------------------------------------------
-- brute force approach by computing all combinations beforehand(Discarded)
----------------------------------------------------------------------------
-- subsequencesOfSize :: Int -> [a] -> [[a]]
-- subsequencesOfSize n xs = let l = length xs
--                           in if n>l then [] else subsequencesBySize xs !! (l-n)

-- subsequencesBySize :: [t] -> [[[t]]]                          
-- subsequencesBySize [] = [[[]]]
-- subsequencesBySize (x:xs) = let next = subsequencesBySize xs in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
-- initialGuess :: Int -> ([Card],GameState)

-- initialGuess num = (guess, gamestate)
--     where guess = head (combinations gamestate)
--           gamestate = GameState { combinations= subsequencesOfSize num ([minBound..maxBound] :: [Card]),
--                                   prevFeedback = (0, 0, 0, 0, 0),
--                                   prevGuess=[]}
