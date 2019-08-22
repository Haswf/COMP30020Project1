module Proj1 (Pitch, toPitch, feedback, removeJust, compareNote, compareOctave, Octave, Note) where
import Data.Maybe
import Data.List

data Octave = One | Two | Three deriving (Read, Eq)
data Note = A | B | C | D | E | F | G deriving (Show, Read, Eq)
data Pitch = Pitch Note Octave deriving (Read, Eq)

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

-- convert a list of Maybe Pitch to Pitch
removeJust :: [Maybe Pitch] -> [Pitch]
removeJust = map fromJust


-- comparePitch :: Pitch -> Pitch -> Bool
-- comparePitch p1 p2
--     | getNote p1 == getNote p2 && getOctave p1 == getOctave p2 = True
--     | otherwise = False

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

-- countEq :: [a] -> [a] -> (a -> a -> Bool) -> Int
-- countEq [] _ _ = 0
-- countEq _ [] _ = 0
-- countEq (x:xs) (y:ys) func
--    | func x y = 1 + countEq xs ys func
--    | otherwise = 0 + countEq xs ys func

-- feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
