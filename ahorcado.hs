import System.IO -- Required for the hSetEcho primitive
import Control.Monad (when)

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word 0

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


play :: String -> Int -> IO ()
play word incorrectCount = do
  when (incorrectCount >= maxGuesses) $ do
    displayHangman incorrectCount
    putStrLn "You've run out of guesses!"
    putStrLn $ "The word was: " ++ word
    return ()

  displayHangman incorrectCount
  putStr "? "
  guess <- getLine
  if guess == word then
    putStrLn "You got it!"
  else
    let
      newIncorrectCount = if match word guess == map (const '-') word
                          then incorrectCount + 1
                          else incorrectCount
      feedback = match word guess
    in do
      putStrLn feedback
      putStrLn $ "Incorrect guesses left: " ++ show (maxGuesses - newIncorrectCount)
      play word newIncorrectCount


match :: String -> String -> String
match xs ys =
   [if elem x ys then x else '-' | x <- xs]