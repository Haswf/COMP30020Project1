module Proj1 (Pitch, toPitch, compareNote, compareOctave, Octave, Note, feedback, GameState) where
import Data.Maybe
import Data.List

data Octave = One | Two | Three deriving (Read, Eq, Enum, Bounded)
data Note = A | B | C | D | E | F | G deriving (Show, Read, Eq, Enum, Bounded)
data Pitch = Pitch Note Octave deriving (Read, Eq)
data GameState = GameState { remaining :: [SolutionSpace]
                           , prevFeedback :: (Int, Int, Int)
} deriving Show

-- A collection of all possible pitchs groupby note
type SolutionSpace = [Pitch]

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
   -- | (getOctave p1 /= getOctave p2) && 
    | getNote p1 == getNote p2 = True
    | otherwise = False

-- function to compare if two pitchs have the same octave but different note
compareOctave :: Pitch -> Pitch -> Bool
compareOctave p1 p2 
 --   | (getNote p1 /= getNote p2) &&
    | getOctave p1 == getOctave p2 = True
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
initialSolutionSpace = [Pitch note octave |
                                         note <- [A .. G],              
                                         octave <- [One .. Three]]


-- Extract next element from a SolutionSpace
next :: SolutionSpace -> Pitch
next sp = head sp

-- Create inisital guess 
initialGuess :: ([Pitch],GameState)
initialGuess = (guess, gamestate)
                where fullCombination = [initialSolutionSpace, initialSolutionSpace, initialSolutionSpace]
                      guess = map next fullCombination -- for each SolutionSpace, extract next pitch
                      gamestate = GameState {remaining=fullCombination,
                                             prevFeedback=(0,0,0)
                                            }


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


-- Compare feedback from two guesses. Indicates how 
-- SolutionSpace should be updated.
compareFeedback :: (Int, Int, Int) -> (Int, Int, Int) -> Signal
compareFeedback (prevMatch, prevNotes, prevOctaves) (thisMatch, thisNotes, thisOctaves) 
    | thisMatch > prevMatch = KeepPitch
    | thisNotes > prevNotes = KeepNote       -- K
    | thisOctaves > prevOctaves = KeepOctave 
    | otherwise = Discard -- Discard all pitches with the same note and the same octave

-- Update SolutionSpace for a single pitch
updateSolutionSpace :: Signal -> Pitch -> SolutionSpace -> SolutionSpace
updateSolutionSpace sig this oldSp 
    | sig == KeepPitch = keepPitch this oldSp
    | sig == KeepNote = keepNote this oldSp
    | sig == KeepNote = keepPitch this oldSp
    -- TODO: Rewrite this with function composition
    | sig == Discard = delNote this (delOctave this oldSp ) 

-- Clear solution space as the right one has been found.
keepPitch :: Pitch -> SolutionSpace -> SolutionSpace
keepPitch corr sp = [corr]

-- filter out pitchs that do NOT have the same note as given pitch
keepNote :: Pitch -> SolutionSpace -> SolutionSpace
keepNote pitch sp = filter (compareNote pitch) sp

-- filter out pitchs that do NOT have the same octave  as given pitch
keepOctave :: Pitch -> SolutionSpace -> SolutionSpace
keepOctave pitch sp = filter (compareOctave pitch) sp

-- delete all pitches that have the same note as given pitch from SolutionSpace
delNote :: Pitch -> SolutionSpace -> SolutionSpace
delNote pitch sp = sp \\ filter (compareNote pitch) sp

-- delete all pitches that have the same octave as given pitch from SolutionSpace
delOctave :: Pitch -> SolutionSpace -> SolutionSpace
delOctave pitch sp = sp \\ filter (compareOctave pitch) sp

-- nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
-- nextGuess (thisGuess, gs) thisFeedback