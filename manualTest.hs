retval = initialGuess (length answer)
guess = getGuess retval
gamestate = getGameState retval
findWrong (guess, gamestate) (feedback answer guess)


ans = (read "[AH,2C,3C]") :: [Card]
retval = testFindWrong ans

guess = getGuess retval
gamestate = getGameState retval
retval = findWrong (guess, gamestate) (feedback ans guess)
retval

