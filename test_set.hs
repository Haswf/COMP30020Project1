import Proj1
import Data.Maybe
import Data.List

generateInput :: [String] -> [Pitch]
generateInput input = removeJust (map toPitch input)

-- test :: [Pitch] -> (Int, Int, Int)
-- let x = generateInput ["A1","B2","A3"]
