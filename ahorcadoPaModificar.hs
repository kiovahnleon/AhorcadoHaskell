import System.IO -- modulo necesario para operaciones de entrada/salida. Estamos usando hSetEcho de System.IO
import Control.Monad (when)
import System.Exit (exitSuccess) -- para terminar el programa sin error

hangmanStages :: [String] -- inicializamos una lista de strings para representar visualmente al monito
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
maxGuesses = length hangmanStages - 1  -- Menos 1 porque indices comienzan en 0

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
    displayHangman incorrectCount  -- mostrar monito en forma final
    putStrLn "Se te acabaron los intentos :c"
    putStrLn $ "La palabra era: " ++ word
    exitSuccess

  displayHangman incorrectCount
  putStrLn $ "Lo que llevas adivinado: " ++ map (\c -> if c `elem` guessedLetters then c else '_') word
  putStrLn "Adivina una letra: "
  guess <- getLine
  let guessChar = if null guess then ' ' else head guess
  if guessChar `elem` word then
    putStrLn "Correcto!"
  else
    putStrLn "Incorrecto!"
  let newGuessedLetters = if guessChar `elem` word then guessChar : guessedLetters else guessedLetters
  let newIncorrectCount = if guessChar `elem` word then incorrectCount else incorrectCount + 1
  if all (`elem` newGuessedLetters) word then do
    putStrLn $ "Felicidades jugador 2! Haz adivinado la palabra: " ++ word
    exitSuccess
  else
    play word newGuessedLetters newIncorrectCount

main :: IO ()
main = do
  putStrLn "Jugador 1, escribe una palabra: "
  word <- sgetLine
  putStrLn "Jugador 2, adivina la palabra, una letra a la vez: "
  play word "" 0