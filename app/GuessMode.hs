module GuessMode where

import Data.Map (Map)
import qualified Data.Map as Map

data Square = Gray | Yellow | Green deriving (Eq, Ord)

instance Show Square where
  show Gray = "⬜"
  show Yellow = "🟨"
  show Green = "🟩"

compareWordsHelper :: String -> String -> String -> [Square]
compareWordsHelper _ [] [] = []
compareWordsHelper _ [] _ = []
compareWordsHelper _ _ [] = []
compareWordsHelper originalGuessWord (x : xs) (y : ys)
  | x == y = Green : compareWordsHelper originalGuessWord xs ys
  | y `elem` originalGuessWord = Yellow : compareWordsHelper originalGuessWord xs ys
  | otherwise = Gray : compareWordsHelper originalGuessWord xs ys

compareWords :: String -> String -> [Square]
compareWords guessWord = compareWordsHelper guessWord guessWord

playGuessModeNormal :: Int -> String -> IO ()
playGuessModeNormal 6 word = do
  putStrLn ("You ran out of guesses. The word was: " ++ word)
playGuessModeNormal turn word = do
  putStrLn "Enter your word guess: "
  inputWord <- getLine
  if length inputWord /= length word
    then do
      putStrLn "Invalid input length"
      return ()
    else
      if inputWord == word
        then do
          mapM_ (\_ -> putStr (show Green)) word
          putStrLn ""
        else do
          mapM_ (putStr . show) (compareWords word inputWord)
          putStrLn ""
          playGuessModeNormal (turn + 1) word

handleLettersMap :: Map (Char, Int) Square -> [((Char, Int), Square)] -> Map (Char, Int) Square
handleLettersMap _ [] = Map.empty
handleLettersMap currMap sl@(x : xs)
  | null currMap = Map.fromList sl
  | null sl = currMap
  | otherwise = handleLettersMap (uncurry Map.insert x currMap) xs

playGuessModeEasy :: Map (Char, Int) Square -> [String] -> Int -> String -> IO ()
playGuessModeEasy _ _ 6 word = do
  putStrLn ("You ran out of guesses. The word was: " ++ word)
playGuessModeEasy letters dict turn word = do
  putStrLn "Enter your word guess: "
  inputWord <- getLine
  if length inputWord /= length word
    then do
      putStrLn "Invalid input length"
      return ()
    else
      if inputWord == word
        then do
          mapM_ (\_ -> putStr (show Green)) word
          putStrLn ""
        else do
          let isInDict = inputWord `elem` dict
          if isInDict then putStr "" else putStrLn "The word you gave as an input is not in the dictionary!"
          let currGuess = compareWords word inputWord
          let indexedLetters = zip inputWord [0 ..]
          let squaresLetters = zip indexedLetters currGuess

          if null letters
            then do
              mapM_ (putStr . show) (compareWords word inputWord)
              putStrLn ""
              playGuessModeNormal (turn + 1) word
            else do
              let grayLetters = foldr (\x acc -> if Map.lookup x letters == Just Gray then fst x : acc else acc) [] indexedLetters
              if null grayLetters
                then do
                  putStr ""
                else do
                  putStr "The following letters are known to be gray: "
                  mapM_ (\x -> putStr (show x ++ " ")) grayLetters
              -- TODO: add other checks
              let letters' = handleLettersMap letters squaresLetters
              mapM_ (putStr . show) (compareWords word inputWord)
              putStrLn ""
              playGuessModeNormal (turn + 1) word

playGuessModeExpert 6 word = do
  putStrLn ("You ran out of guesses. The word was: " ++ word)
playGuessModeExpert turn word = undefined