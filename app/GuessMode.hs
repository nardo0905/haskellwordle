module GuessMode where

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
      putStrLn "Invalid input"
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

playGuessModeEasy 6 word = do
  putStrLn ("You ran out of guesses. The word was: " ++ word)
playGuessModeEasy turn word = undefined

playGuessModeExpert 6 word = do
  putStrLn ("You ran out of guesses. The word was: " ++ word)
playGuessModeExpert turn word = undefined