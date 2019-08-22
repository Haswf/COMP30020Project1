import Proj1
import Data.Maybe

generateInput :: [String] -> [Maybe Pitch]
generateInput = map toPitch

-- test :: [Pitch] -> (Int, Int, Int)
