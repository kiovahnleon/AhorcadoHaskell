import System.IO -- modulo necesario para operaciones de entrada/salida. Estamos usando hSetEcho de System.IO
import Control.Monad (when)
import System.Exit (exitSuccess) -- para terminar el programa sin error

hangmanStages :: [String] -- inicializamos una lista de strings para representar visualmente al monito
hangmanStages = [
    "  +---+\n  |   |\n      |\n      |\n      |\n      |\n=========", -- 0 Fallos
    "  +---+\n  |   |\n  O   |\n      |\n      |\n      |\n=========", -- 1 Fallo
    "  +---+\n  |   |\n  O   |\n  |   |\n      |\n      |\n=========", -- 2 Fallos
    "  +---+\n  |   |\n  O   |\n /|   |\n      |\n      |\n=========", -- 3 Fallos
    "  +---+\n  |   |\n  O   |\n /|\\  |\n      |\n      |\n=========", -- 4 Fallos
    "  +---+\n  |   |\n  O   |\n /|\\  |\n /    |\n      |\n=========", -- 5 Fallos
    "  +---+\n  |   |\n  O   |\n /|\\  |\n / \\  |\n      |\n=========" -- 6 Fallos
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

play :: String -> String -> [Char] -> Int -> IO ()
play word guessedLetters incorrectGuesses incorrectCount = do
  when (incorrectCount >= maxGuesses) $ do
    displayHangman incorrectCount
    putStrLn "Se te acabaron los intentos :c"
    putStrLn $ "La palabra era: " ++ word
    exitSuccess

  displayHangman incorrectCount
  putStrLn $ "Lo que llevas adivinado: " ++ map (\c -> if c `elem` guessedLetters then c else '_') word
  putStrLn "Adivina una letra: "
  guess <- getLine
  let guessChar = if null guess then ' ' else head guess
  if guessChar `elem` guessedLetters || guessChar `elem` incorrectGuesses then do
    putStrLn "Ya adivinaste esa letra o la intentaste antes. Intenta con otra."
    play word guessedLetters incorrectGuesses incorrectCount
  else if guessChar `elem` word then do
    putStrLn "Correcto!"
    let newGuessedLetters = guessChar : guessedLetters
    if all (`elem` newGuessedLetters) word then do
      putStrLn $ "Felicidades jugador 2! Haz adivinado la palabra: " ++ word
      exitSuccess
    else
      play word newGuessedLetters incorrectGuesses incorrectCount
  else do
    putStrLn "Incorrecto!"
    let newIncorrectGuesses = guessChar : incorrectGuesses
    let newIncorrectCount = incorrectCount + 1
    play word guessedLetters newIncorrectGuesses newIncorrectCount

main :: IO ()
main = do
  putStrLn "Jugador 1, escribe una palabra: "
  word <- sgetLine
  putStrLn "Jugador 2, adivina la palabra, una letra a la vez: "
  play word "" [] 0