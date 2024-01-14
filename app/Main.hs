{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import GenerateWord
import GuessMode
import HelpMode
import System.Random (getStdRandom, randomR)
import Text.Read (readMaybe)

-- github repo на проекта: https://github.com/nardo0905/haskellwordle
-- документацията на проекта може да бъде намерена във файла README.md

main :: IO ()
main = do
  putStrLn "% Choose a game mode: 'guess' or 'help' or type 'quit' to quit: "
  gameMode <- getLine
  case gameMode of
    "guess" -> do
      putStrLn "% Choose a level of difficulty: 'easy', 'normal' or 'expert' or 'back' to go back to mode selection"
      lod <- getLine
      case lod of
        "easy" -> do
          putStrLn "% Input a word length: "
          wl <- getLine
          let wlToInt = readMaybe wl :: Maybe Int
          case wlToInt of
            Just num -> do
              wordToGuess <- generateWord num
              dict <- filterWordList num
              -- putStrLn wordToGuess
              putStrLn "-------------------------------"
              playGuessModeEasy Map.empty dict 0 wordToGuess
            _ -> do
              putStrLn "Invalid input!"
              main
        "normal" -> do
          putStrLn "% Input a word length: "
          wl <- getLine
          let wlToInt = readMaybe wl :: Maybe Int
          case wlToInt of
            Just num -> do
              wordToGuess <- generateWord num
              -- putStrLn wordToGuess
              putStrLn "-------------------------------"
              playGuessModeNormal 0 wordToGuess
            _ -> do
              putStrLn "Invalid input!"
              main
        "expert" -> do
          putStrLn "% Input a word length: "
          wl <- getLine
          let wlToInt = readMaybe wl :: Maybe Int
          case wlToInt of
            Just num -> do
              wordToGuess <- generateWord num
              lieRound <- getStdRandom (randomR (0, 5))
              -- putStrLn wordToGuess
              putStrLn "-------------------------------"
              playGuessModeExpert lieRound Map.empty 0 wordToGuess
            _ -> do
              putStrLn "Invalid input!"
              main
        "back" -> main
        _ -> do
          putStrLn "Invalid input! Try again."
          main
    "help" -> do
      putStrLn "% Input a word length: "
      wl <- getLine
      let wlToInt = readMaybe wl :: Maybe Int
      case wlToInt of
        Just num -> do
          wordToGuess <- generateWord num
          dict <- filterWordList num
          putStrLn ("The word the bot has to guess is: " ++ wordToGuess)
          putStrLn "-------------------------------"
          playHelpMode dict 0 wordToGuess
        _ -> do
          putStrLn "Invalid input!"
          main
    "quit" -> return ()
    _ -> do
      putStrLn "Invalid input! Try again."
      main
