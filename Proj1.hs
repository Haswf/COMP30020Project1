module Proj1 (Pitch, toPitch, compareNote, compareOctave, Octave, Note, feedback, GameState) where
import Data.Maybe
import Data.List

data Octave = One | Two | Three deriving (Read, Eq, Enum, Bounded)
data Note = A | B | C | D | E | F | G deriving (Show, Read, Eq, Enum, Bounded)
data Pitch = Pitch Note Octave deriving (Read, Eq)
data GameState = GameState { remaining :: [SolutionSpace]
                           , prevFeedback :: (Int, Int, Int)
                           , prevGuess :: [Pitch]
} deriving Show

-- A collection of all possible pitchs groupby note
type SolutionSpace = [[Pitch]]

-- Show instance for Octave. Convert ocatve to String
instance Show Octave where
    show One = "1"
    show Two = "2"
    show Three = "3"

-- Show instance for Pitch. Return two-letter representation of pitch
instance Show Pitch where
    show (Pitch _Note _Octave) = show _Note ++ show _Octave

-- helper function to convert Char to a note    
readNote :: Char -> Maybe Note
readNote s  
    -- Validate note value
    | s `elem` "ABCDEFG" = Just (read [s] :: Note)
    -- otherwise return nothing
    | otherwise = Nothing

-- helper function to convert Char to a octave    
readOctave :: Char -> Maybe Octave
readOctave num 
    | num == '1' = Just One
    | num == '2' = Just Two
    | num == '3' = Just Three
    | otherwise = Nothing

-- Function to read Pitch from string
toPitch :: String -> Maybe Pitch
toPitch str 
    -- validate octave and note have valid value
    | isJust octave && isJust note = Just (Pitch (fromJust note) (fromJust octave))
    -- otherwise, return nothing
    | otherwise = Nothing
    -- extract octave and note by indexing
    where octave = readOctave (str !! 1)
          note = readNote (str !! 0)

-- return note of a pitch
getNote :: Pitch -> Note
getNote (Pitch n _) = n

-- return octave of a pitch
getOctave :: Pitch -> Octave
getOctave (Pitch _ o) = o

-- function to compare if two pitchs have the same note but different octave
compareNote :: Pitch -> Pitch -> Bool
compareNote p1 p2 
    | (getOctave p1 /= getOctave p2) && (getNote p1 == getNote p2) = True
    | otherwise = False

-- function to compare if two pitchs have the same octave but different note
compareOctave :: Pitch -> Pitch -> Bool
compareOctave p1 p2 
    | (getNote p1 /= getNote p2) && (getOctave p1 == getOctave p2) = True
    | otherwise = False

-- Providing feedback on the number of matching pitch\notes\ocataves.
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback guess target = (length intersection, 
                         length (intersectBy compareNote uniqueTarget uniqueGuess),
                         length (intersectBy compareOctave uniqueTarget uniqueGuess))
    where intersection = guess `intersect` target 
          uniqueTarget = target \\ intersection
          uniqueGuess = guess \\ intersection

-- Read pitch(s) from a list of String
readInputFromList :: [String] -> [Pitch]
-- Filter out Nothing in the list
readInputFromList input = map (fromJust . toPitch) input

-- Initialise a solution space which contains all possible pitches
initialSolutionSpace :: SolutionSpace
initialSolutionSpace = groupBy compareNote [Pitch note octave |
                                         note <- [A .. G],              
                                         octave <- [One .. Three]]


-- Create inisital guess 
initialGuess :: ([Pitch],GameState)
initialGuess = (guess, gamestate)
                where fullCombination = [initialSolutionSpace, initialSolutionSpace, initialSolutionSpace]
                      guess = (map nextPitch fullCombination) -- for each SolutionSpace, extract next pitch
                      gamestate = GameState {remaining=fullCombination, prevFeedback=(0,0,0), prevGuess=guess}

-- Extract next element from a SolutionSpace
nextPitch :: SolutionSpace -> Pitch
nextPitch comb = head (head comb)

-- Diff function to compare two guesses. 
-- Can be used to find which card results in change in feedback.
diff :: [Pitch] -> [Pitch] -> [Pitch]
diff prev this = this \\ prev


-- Signal is defined to manipulate game state
data Signal = KeepPitch -- found matching pitch
    | KeepNote          -- found matching note, change octace
    | KeepOctave        -- found matching octave, change note
    | Discard           -- No luck
    deriving (Show, Eq)


-- compareFeedback compares feedback of two guesses.
-- send a signal on how to change game state.
compareFeedback :: (Int, Int, Int) -> (Int, Int, Int) -> Signal
compareFeedback (prevMatch, prevNotes, prevOctaves) (thisMatch, thisNotes, thisOctaves) 
    | thisMatch > prevMatch = KeepPitch
    | thisNotes > prevNotes = KeepNote
    | thisOctaves > prevOctaves = KeepOctave
    | otherwise = Discard

-- TODO: To be implemented
-- updateGameState :: ([Pitch], GameState) -> Signal -> [Pitch]
-- updateGameState (guess, old) signal
--     | signal == KeepPitch = changeElem
--         where changeElem = diff guess (prevGuess old)