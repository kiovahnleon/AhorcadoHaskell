import System.IO -- Required for the hSetEcho primitive
import Control.Monad (when)
import System.Exit (exitSuccess)

hangmanStages :: [String]
hangmanStages = [
    "  +---+\n  |   |\n      |\n      |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n      |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n  |   |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|   |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|\\  |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|\\  |\n /    |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|\\  |\n / \\  |\n      |\n========="
  ]

maxGuesses :: Int
maxGuesses = length hangmanStages - 1  -- Minus 1 because indexes start at 0

displayHangman :: Int -> IO ()
displayHangman incorrectCount = putStrLn (hangmanStages !! incorrectCount)

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)


getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x


play :: String -> String -> Int -> IO ()
play word guessedLetters incorrectCount = do
  when (incorrectCount >= maxGuesses) $ do
    displayHangman incorrectCount  -- Show the final hangman stage
    putStrLn "You've run out of guesses!"
    putStrLn $ "The word was: " ++ word
    exitSuccess

  displayHangman incorrectCount
  putStrLn $ "Guessed so far: " ++ map (\c -> if c `elem` guessedLetters then c else '_') word
  putStrLn "Guess a letter: "
  guess <- getLine
  let guessChar = if null guess then ' ' else head guess
  if guessChar `elem` word then
    putStrLn "Correct!"
  else
    putStrLn "Incorrect!"
  let newGuessedLetters = if guessChar `elem` word then guessChar : guessedLetters else guessedLetters
  let newIncorrectCount = if guessChar `elem` word then incorrectCount else incorrectCount + 1
  if all (`elem` newGuessedLetters) word then do
    putStrLn $ "Congratulations! You've guessed the word: " ++ word
    exitSuccess
  else
    play word newGuessedLetters newIncorrectCount


main :: IO ()
main = do
  putStrLn "Think of a word: "
  word <- sgetLine
  putStrLn "Try to guess it, one letter at a time."
  play word "" 0