module Spec where

import Data.Maybe (catMaybes)
import Test.Hspec
import Test.QuickCheck
import Main
  (Puzzle(..), fillInCharacter, handleGuess)

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "adds 'p' to \"puzzle\"" $ do
      fillInCharacter
        (Puzzle "puzzle"
                [ Nothing
                , Just 'u'
                , Just 'z'
                , Just 'z'
                , Just 'l'
                , Just 'e'
                ]
                "uzle")
                'p'
      `shouldBe`
        (Puzzle "puzzle" (fmap Just "puzzle") "puzle")
    it "always adds a character \
       \to an incomplete puzzle" $ do
      property prop_buildsCharacter
    it "adds a character not \
       \in the list of all attempts" $ do
      property prop_addsNewCharacter
  describe "handleGuess" $ do
    it "adds a bad guess only to \
       \allGuesses in the puzzle" $ do
      property (again prop_addsBadGuessToAllGuessesOnly)
    it "does nothing with a guess \
       \that was already attempted" $ do
      property (again prop_noModifyKnownGuess)

newtype IncompletePuzzle = IncompletePuzzle Puzzle
  deriving (Eq, Show)

toIncomplete :: Puzzle -> Maybe IncompletePuzzle
toIncomplete p@(Puzzle w _ already)
  | all (flip elem already) w = Nothing
  | otherwise = Just (IncompletePuzzle p)

instance Arbitrary IncompletePuzzle where
  arbitrary = genIncompletePuzzle

genIncompletePuzzle :: Gen IncompletePuzzle
genIncompletePuzzle =
  suchThatMap (arbitrary :: Gen Puzzle) toIncomplete

goodNewGuess :: IncompletePuzzle -> Gen Char
goodNewGuess (IncompletePuzzle (Puzzle word good _)) = do
  let possible =
        filter (not . (flip elem . catMaybes $ good)) word
  elements possible

genIPuzAndGoodGuess :: Gen (IncompletePuzzle, Char)
genIPuzAndGoodGuess = do
  puz <- arbitrary :: Gen IncompletePuzzle
  ch  <- goodNewGuess puz
  return (puz, ch)

genIPuzAndBadGuess :: Gen (IncompletePuzzle, Char)
genIPuzAndBadGuess = do
  puz <- arbitrary :: Gen IncompletePuzzle
  ch  <- elements ['1'..'9']
  return (puz, ch)

genIPuzAndTriedGuess :: Gen (IncompletePuzzle, Char)
genIPuzAndTriedGuess = do
  puz@(IncompletePuzzle (Puzzle _ _ a')) <-
    suchThat arbitrary
      (\(IncompletePuzzle (Puzzle _ _ a)) ->
         not . null $ a)
  ch <- elements a'
  return (puz, ch)

prop_addsNewCharacter :: Property
prop_addsNewCharacter =
  forAll genIPuzAndGoodGuess $
    \(IncompletePuzzle puz, ch) ->
      addsNew (fillInCharacter puz ch) puz
  where
    addsNew (Puzzle _ _ (n:_))
            (Puzzle _ _  oldAll) =
      not (elem n oldAll)
    addsNew _ _ = True

prop_buildsCharacter :: Property
prop_buildsCharacter =
  forAll genIPuzAndGoodGuess $
    \(IncompletePuzzle puz, ch) ->
      isFuller (fillInCharacter puz ch) puz
  where
    isFuller (Puzzle _ newGood newAll)
             (Puzzle _ oldGood oldAll) =
      (length $ catMaybes newGood)
      > (length $ catMaybes oldGood)
      &&
      length newAll > length oldAll

prop_addsBadGuessToAllGuessesOnly :: Property
prop_addsBadGuessToAllGuessesOnly = ioProperty $ do
  (IncompletePuzzle puz, ch):_ <-
    sample' genIPuzAndBadGuess
  newPuz <- handleGuess puz ch
  return $ checkGuess ch newPuz puz
  where
    checkGuess c (Puzzle w goodNew allNew)
                 (Puzzle _ _       allOld) =
         elem c allNew
      && (not . elem c $ allOld)
      && (not . elem (Just c) $ goodNew)
      && (not . elem c $ w)


prop_noModifyKnownGuess :: Property
prop_noModifyKnownGuess = ioProperty $ do
  (IncompletePuzzle puz, ch):_ <-
    sample' genIPuzAndTriedGuess
  newPuz <- handleGuess puz ch
  return (newPuz == puz)
