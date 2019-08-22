import Data.Maybe

data Octave = One | Two | Three deriving (Show, Read, Eq)
data Note = A | B | C | D | E | F | G deriving (Show, Read, Eq)
data Pitch = Pitch Note Octave deriving (Show, Read, Eq)


readNote :: Char -> Maybe Note
readNote s  
    | s `elem` "ABCDEFG" = Just (read [s] :: Note)
    | otherwise = Nothing

readOctave :: Int -> Maybe Octave
readOctave num 
    | num == 1 = Just One
    | num == 2 = Just Two
    | num == 3 = Just Three
    | otherwise = Nothing

toPitch :: String -> Maybe Pitch
toPitch str 
    | isJust octave && isJust note = Just (Pitch (fromJust note) (fromJust octave))
    | otherwise = Nothing
    where octave = readOctave (read [str !! 1] :: Int)
          note = readNote (str !! 0)

getNote :: Pitch -> Note
getNote (Pitch n o) = n

getOctave :: Pitch -> Octave
getOctave (Pitch n o) = o

countCorrectOctave :: [Pitch] -> [Pitch] -> Int
countCorrectOctave (x:xs) (y:ys) 
    | getOctave x == getOctave y = 1 + countCorrectOctave xs ys
    | otherwise = 0 + countCorrectOctave xs ys

compareNote :: Pitch -> Pitch -> Bool
compareNote p1 p2 
    | getNote p1 == getNote p2 = True
    | otherwise = False

compareOctave :: Pitch -> Pitch -> Bool
compareOctave p1 p2 
    | getOctave p1 == getOctave p2 = True
    | otherwise = False

removeJust :: [Maybe Pitch] -> [Pitch]
removeJust = map fromJust

comparePitch :: Pitch -> Pitch -> Bool
comparePitch p1 p2
    | getNote p1 == getNote p2 && getOctave p1 == getOctave p2 = True
    | otherwise = False

countEq :: [a] -> [a] -> (a -> a -> Bool) -> Int
countEq (x:xs) (y:ys) func
    | func x y = 1 + countEq xs ys func
    | otherwise = 0 + countEq xs ys func

feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (countEq target guess comparePitch, countEq target guess compareNote, countEq target guess compareOctave)