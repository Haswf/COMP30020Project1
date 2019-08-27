import Proj1
import Data.Maybe
import Data.List

generateInput :: [String] -> [Pitch]
generateInput input = removeJust (map toPitch input)

-- test :: [Pitch] -> (Int, Int, Int)
-- let x = generateInput ["A1","B2","A3"]

let target = [fromJust (toPitch "A3")]
let sp = initialSolutionSpace 
let guess = [nextPitch sp]
feedback target guess

let x = readFromList ["A1", "B2", "A3"]
let y = readFromList ["A1", "A2", "B1"]