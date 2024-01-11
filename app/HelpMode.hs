module HelpMode where

import GuessMode (Square, compareWords)
import System.Random (getStdRandom, randomR)

-- puskame compareword na vsichki dumi ot dict i filtrirame samo tezi koito vrushtat sushtite kvadratcheta kto sqListRead

playHelpMode :: [String] -> Int -> String -> IO ()
playHelpMode _ 6 _ = do
  putStrLn "I couldn't guess the word in 6 turns :("
playHelpMode dict turn word = do
  -- doing it with a random word from this for now
  wordBotGuessPos <- getStdRandom (randomR (0, length dict - 1))
  let wordBotGuess = dict !! wordBotGuessPos
  putStrLn wordBotGuess
  if wordBotGuess == word
    then do
      putStrLn "The bot guessed the word!"
      return ()
    else do
      putStrLn "Input a list containing Squares"
      sqList <- getLine
      let sqListRead = map read (words sqList) :: [Square]
      let filteredDict = [x | x <- dict, compareWords word x == sqListRead]
      playHelpMode filteredDict (turn + 1) word