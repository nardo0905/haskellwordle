module GenerateWord (generateWord) where

import System.Random (Random (randomR), getStdRandom)

-- This function takes a file with words and filters only the words with length n in it and puts them in a list
filterWordList :: Int -> IO [String]
filterWordList n = do
  wordFile <- readFile "data/words_alpha.txt"
  let wordsList = words wordFile
  let filteredList = filter (\x -> length x == n) wordsList
  return filteredList

-- generates a random word with length n

generateWord :: Int -> IO String
generateWord n = do
  let wordList = filterWordList n
  wordList' <- wordList
  wordPos <- getStdRandom (randomR (0, length wordList'))
  return $ wordList' !! wordPos
