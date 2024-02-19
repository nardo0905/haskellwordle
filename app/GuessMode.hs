module GuessMode where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (getStdRandom, randomR)

data Square = Gray | Yellow | Green deriving (Eq, Ord)

instance Show Square where
  show Gray = "⬜"
  show Yellow = "🟨"
  show Green = "🟩"

instance Read Square where
  readsPrec _ value =
    tryParse [("Green", Green), ("Yellow", Yellow), ("Gray", Gray)]
    where
      tryParse [] = []
      tryParse ((attempt, result) : xs) =
        if take (length attempt) value == attempt
          then [(result, drop (length attempt) value)]
          else tryParse xs

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
  putStrLn "\n% Enter your word guess: "
  inputWord <- getLine
  putStrLn "-------------------------------"
  if length inputWord /= length word
    then do
      putStrLn "Invalid input length"
      return ()
    else do
      putStrLn $ concatMap (: " ") inputWord
      if inputWord == word
        then do
          mapM_ (\_ -> putStr (show Green)) word
          putStrLn []
          putStrLn "Correct guess!"
        else do
          mapM_ (putStr . show) (compareWords word inputWord)
          putStrLn []
          playGuessModeNormal (turn + 1) word

handleLettersMap :: Map Char (Int, Square) -> [(Char, (Int, Square))] -> Map Char (Int, Square)
handleLettersMap currMap [] = currMap
handleLettersMap currMap sl@(x : xs)
  | null currMap = Map.fromList sl
  | null sl = currMap
  | otherwise = handleLettersMap (uncurry Map.insert x currMap) xs

checkIfGray :: Maybe (Int, Square) -> Bool
checkIfGray sqPair = case sqPair of
  Nothing -> False
  Just (_, col) -> col == Gray

playGuessModeEasy :: Map Char (Int, Square) -> [String] -> Int -> String -> IO ()
playGuessModeEasy _ _ 6 word = do
  putStrLn ("You ran out of guesses. The word was: " ++ word)
playGuessModeEasy letters dict turn word = do
  -- print letters
  putStrLn "\n% Enter your word guess: "
  inputWord <- getLine
  putStrLn "-------------------------------"
  if length inputWord /= length word
    then do
      putStrLn "Invalid input length"
      playGuessModeEasy letters dict turn word
      return ()
    else do
      if inputWord == word
        then do
          putStrLn $ concatMap (: " ") inputWord
          mapM_ (\_ -> putStr (show Green)) word
          putStrLn []
          putStrLn "Correct guess!"
        else do
          let isInDict = inputWord `elem` dict
          if isInDict then putStr [] else putStrLn "- The word you gave as an input is not in the dictionary!"
          let currGuess = compareWords word inputWord
          let indexedSquares = zip [0 ..] currGuess
          let squaresLetters = zip inputWord indexedSquares

          let grayLetters = nub $ foldr (\x acc -> if checkIfGray (Map.lookup x letters) then x : acc else acc) [] inputWord
          if null grayLetters
            then do
              putStr []
            else do
              putStr "- The following letters are known to be gray, but are in your guess: "
              mapM_ (\x -> putStr (show x ++ " ")) grayLetters
              putStrLn []

          let yellowsNotInWord = nub $ foldr (\(letter, (_, col)) acc -> if col == Yellow && letter `notElem` inputWord then letter : acc else acc) [] (Map.toList letters)
          if null yellowsNotInWord
            then do
              putStr []
            else do
              putStr "- The following letters are known to be yellow, but are not in your guess: "
              mapM_ (\x -> putStr (show x ++ " ")) yellowsNotInWord
              putStrLn []

          let greenPositionsNotInUse = nub $ foldr (\(letter, (ind, col)) acc -> if col == Green && (inputWord !! ind) /= letter then (ind + 1) : acc else acc) [] (Map.toList letters)
          if null greenPositionsNotInUse
            then do
              putStr []
            else do
              putStr "- You already know that there is a green letter at the following positions:  "
              mapM_ (\x -> putStr (show x ++ " ")) greenPositionsNotInUse
              putStrLn []

          let letters' = handleLettersMap letters squaresLetters
          putStrLn $ concatMap (: " ") inputWord
          mapM_ (putStr . show) currGuess
          putStrLn []
          playGuessModeEasy letters' dict (turn + 1) word

createLie :: Int -> Map Char (Int, Square) -> String -> IO [Square]
createLie _ _ [] = return []
createLie currLetterId lettersHistory (x : xs) = do
  let currLetterInHistory = Map.lookup x lettersHistory
  case currLetterInHistory of
    Nothing -> do
      lieSquare <- getStdRandom (randomR (1 :: Int, 3))
      case lieSquare of
        1 -> fmap (Gray :) (createLie (currLetterId + 1) lettersHistory xs)
        2 -> fmap (Yellow :) (createLie (currLetterId + 1) lettersHistory xs)
        3 -> fmap (Green :) (createLie (currLetterId + 1) lettersHistory xs)
        _ -> createLie (currLetterId + 1) lettersHistory xs
    Just (letId, square) ->
      if square == Gray
        then fmap (Gray :) (createLie (currLetterId + 1) lettersHistory xs)
        else
          if square == Yellow
            then fmap (Yellow :) (createLie (currLetterId + 1) lettersHistory xs)
            else
              if square == Green && letId == currLetterId
                then fmap (Green :) (createLie (currLetterId + 1) lettersHistory xs)
                else fmap (Yellow :) (createLie (currLetterId + 1) lettersHistory xs)

playGuessModeExpert :: Int -> Map Char (Int, Square) -> Int -> String -> IO ()
playGuessModeExpert _ _ 6 word = do
  putStrLn ("You ran out of guesses. The word was: " ++ word)
playGuessModeExpert lieRound letters turn word = do
  putStrLn "\n% Enter your word guess: "
  inputWord <- getLine
  putStrLn "-------------------------------"
  if length inputWord /= length word
    then do
      putStrLn "Invalid input length"
      return ()
    else do
      putStrLn $ concatMap (: " ") inputWord
      if inputWord == word
        then do
          mapM_ (\_ -> putStr (show Green)) word
          putStrLn []
          putStrLn "Correct guess!"
        else do
          if turn == lieRound
            then do
              -- putStrLn "should be a lie"
              currLieGuess <- createLie 0 letters inputWord
              mapM_ (putStr . show) currLieGuess
              putStrLn []
              playGuessModeExpert lieRound letters (turn + 1) word
            else do
              let currGuess = compareWords word inputWord
              let indexedSquares = zip [0 ..] currGuess
              let squaresLetters = zip inputWord indexedSquares
              let letters' = handleLettersMap letters squaresLetters
              mapM_ (putStr . show) currGuess
              putStrLn []
              playGuessModeExpert lieRound letters' (turn + 1) word