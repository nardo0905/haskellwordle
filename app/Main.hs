module Main where

import GenerateWord (generateWord)
import GuessMode
import HelpMode

main :: IO ()
main = do
  putStrLn "Choose a game mode: 'guess' or 'help' or type 'quit' to quit: "
  gameMode <- getLine
  case gameMode of
    "guess" -> do
      putStrLn "Choose a level of difficulty: 'easy', 'normal' or 'expert' or 'back' to go back to mode selection"
      lod <- getLine
      case lod of
        "easy" -> do
          putStrLn "Input a word length: "
          wl <- getLine
          wordToGuess <- generateWord (read wl :: Int)
          playGuessModeEasy wordToGuess
        "normal" -> do
          putStrLn "Input a word length: "
          wl <- getLine
          wordToGuess <- generateWord (read wl :: Int)
          playGuessModeNormal wordToGuess
        "expert" -> do
          putStrLn "Input a word length: "
          wl <- getLine
          wordToGuess <- generateWord (read wl :: Int)
          playGuessModeExpert wordToGuess
        "back" -> main
        _ -> return ()
    "help" -> do
      playHelpMode
    _ -> return ()
