{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import GenerateWord
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
          dict <- filterWordList (read wl :: Int)
          putStrLn wordToGuess
          playGuessModeEasy Map.empty dict 1 wordToGuess
        "normal" -> do
          putStrLn "Input a word length: "
          wl <- getLine
          wordToGuess <- generateWord (read wl :: Int)
          -- putStrLn wordToGuess
          playGuessModeNormal 1 wordToGuess
        "expert" -> do
          putStrLn "Input a word length: "
          wl <- getLine
          wordToGuess <- generateWord (read wl :: Int)
          playGuessModeExpert 1 wordToGuess
        "back" -> main
        _ -> return ()
    "help" -> do
      playHelpMode
    _ -> return ()
