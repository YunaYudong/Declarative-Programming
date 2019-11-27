module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where
import Data.List
import Data.Ord

data Pitch = Pitch Note Octave
     deriving (Eq, Ord)
instance Show Pitch where show (Pitch a b) = a:b:[]
type Note = Char
type Octave = Char

-- take two-character string as input, return Just Pitch, or Nothing if it is an invalid input
toPitch :: String -> Maybe Pitch
toPitch (note:octave:[])
    | checkValid = Just (Pitch note octave)
    | otherwise = Nothing
    where checkValid = (note `elem` "ABCDEFG") && (octave `elem` "123")
-- return the note in pitch
note :: Pitch -> Note
note (Pitch note _) = note
-- return the octave in pitch
octave :: Pitch -> Octave
octave (Pitch _ octave) = octave

-- Game State contains all possible remaining guesses with weighted score,
-- take (pitch,score) pair to find the guess with maximum weighted score
type GameState =  [([Pitch],Int)]

-- initial game state contains all the possible chords.
-- each chord consists of three pitches. initialize the score as zero
initialGS :: GameState
initialGS = [((pitch1:pitch2:pitch3:[]),0)|pitch1<-pitch,pitch2<-pitch,pitch3<-pitch,
             pitch1/=pitch2,pitch2/=pitch3,pitch3/=pitch1]
    where pitch = [Pitch note octave|note<-['A'..'G'],octave<-['1'..'3']]

-- take [A1,B2,C3] as initial guess.
-- initial game state with all possible remaining guesses.
initialGuess :: ([Pitch], GameState)
initialGuess = ([Pitch 'A' '1',Pitch 'B' '2',Pitch 'C' '3'],initialGS)

-- compare two inputs and count the match
compare2lst :: Eq a => [a] -> [a] -> Int
compare2lst [] _ = 0
compare2lst _ [] = 0
compare2lst (x:xs) ys
    | x `elem` ys = 1 + compare2lst xs (delete x ys)
    | otherwise = compare2lst xs ys
-- take a target and a guess, returns the number of correct pitch,correct note and correct octave.
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (pitches, notes, octaves)
    where pitches = compare2lst target guess
          notes = compare2lst [note x | x <- target] [note y | y <- guess] - pitches
          octaves = compare2lst [octave x | x <- target] [octave y | y <- guess] - pitches

-- take a guess as input and get the weighted score of this guesses with all possible remaining guesses
-- the score is the sum of correct pitches, correct notes and correct octaves from feedback
-- the weight of correct pitches,correct notes and correct octaves is chosen on the basis of trial between 1 and 10
-- return the total score for the input guess
score :: [Pitch] -> GameState -> Int
score guess [] = 0
score guess (x:xs) = correctCount + (score guess xs)
   where correctCount = 3*cPitch + 7*cNote + cOctave
         (cPitch,cNote,cOctave) = feedback guess (fst x)
-- takes a pair of previous guess and game state as input, and feedback to this guess,
-- returns a pair of next guess and new game state
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (preGuess,preGS) preFeedback = (newGuess,newGS)
    where newGuess = fst $ maximumBy (comparing snd)[((fst x),(score (fst x) newGS))|x<-newGS]
          newGS = [((fst guesses),0)|guesses<-preGS,(feedback preGuess (fst guesses))== preFeedback]



