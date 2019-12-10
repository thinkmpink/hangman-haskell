module Main where

import Control.Monad (forever)
import Data.Char (toLower, isUpper)
import Data.Maybe (isJust, catMaybes)
import Data.List (intersperse, nub)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import System.Random (randomRIO)
import Test.QuickCheck

-- replace this type synonym
-- type WordList = [String]

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList ( filter (not . properNoun)
                    . filter gameLength
                    $ aw )
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
              && l <  maxWordLength
        properNoun (w:_) = isUpper w
        properNoun _     = False

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle
    String       -- the word we're trying to guess
    [Maybe Char] -- characters we've filled in so far
    [Char]       -- letters we've guessed so far
    deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (fmap (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c)
            word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do

  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of

   (_, True) -> do
     putStrLn "You already guessed that\
             \ character, pick \
             \ something else!"
     return puzzle

   (True, _) -> do
     putStrLn "This character was in the\
             \ word, filling in the word\
             \ accordingly"
     return (fillInCharacter puzzle guess)

   (False, _) -> do
     putStrLn "This character wasn't in\
             \ the word, try again."
     return (fillInCharacter puzzle guess)

incorrectCount :: String -> String -> Int
incorrectCount word attempts =
  length . filter (not . flip elem word) $ attempts

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (incorrectCount wordToGuess guessed) > 9 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "

  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must\
              \ be a single character"


instance Arbitrary Puzzle where
  arbitrary = randomPuzzle

genWord :: Gen String
genWord = listOf1 $ elements ['a'..'z']

genSuccessfulGuesses :: String -> Gen ([Maybe Char])
genSuccessfulGuesses correctWord = do
  guessChars <- sublistOf . nub $ correctWord
  return $
    foldr
      (\ch maybeChars ->
          (if elem ch guessChars
           then Just ch
           else Nothing)
          : maybeChars
      ) [] correctWord


randomPuzzle :: Gen Puzzle
randomPuzzle = do
  word           <- genWord
  guessedSoFar   <- genSuccessfulGuesses word
  let allGuesses = nub . catMaybes $ guessedSoFar
  return $ Puzzle word guessedSoFar allGuesses


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle
