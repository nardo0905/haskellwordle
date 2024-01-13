module HelpMode where

import GuessMode (Square, compareWords)
import System.Random (getStdRandom, randomR)

playHelpMode :: [String] -> Int -> String -> IO ()
playHelpMode dict 6 word = do
  wordBotGuessPos <- getStdRandom (randomR (0, length dict - 1))
  let wordBotGuess = dict !! wordBotGuessPos
  putStrLn wordBotGuess
  if wordBotGuess == word
    then do
      putStrLn "The bot guessed the word!"
      return ()
    else do
      putStrLn "I couldn't guess the word in 6 turns :("
playHelpMode dict turn word = do
  wordBotGuessPos <- getStdRandom (randomR (0, length dict - 1))
  let wordBotGuess = dict !! wordBotGuessPos
  putStrLn wordBotGuess
  if wordBotGuess == word
    then do
      putStrLn "The bot guessed the word!"
      return ()
    else do
      putStrLn "% Input a list containing Squares"
      sqList <- getLine
      let sqListRead = map read (words sqList) :: [Square]
      -- putStrLn (show (compareWords word wordBotGuess) ++ " | " ++ show sqListRead)
      if compareWords word wordBotGuess /= sqListRead
        then do
          putStrLn "Invalid input squares!"
          return ()
        else do
          let filteredDict = [x | x <- dict, compareWords x wordBotGuess == sqListRead]
          if null filteredDict
            then do
              putStrLn "No word in the dictionary matches your input!"
              return ()
            else do
              playHelpMode filteredDict (turn + 1) word