{- /
Module		: Proj1
Description	: Project 1 of COMP90048 in 19S1.
Copyright	: Rongbing Shan 945388 @Unimelb
DateCreated	: APR 4th, 2019:

Maintainer	: alex.shan@student.unimelb.edu.au

The code implements bot the composer and performer part of the game of Musician. 
The composer will determine a chord of 3 Pitches, the performer will make guesses
based on the composer's feedback untile performer gets the right chord.
-}

module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where

import Data.List

{- codes below implements the Pitch type,
 - including Notes and Octaves.
 -}
-- | A Pitch Node consisting of A,B, ..,G.
--   Notes are in the Eq, Bounded, Enum, and Read class.
data Note = A|B|C|D|E|F|G
    deriving (Eq, Bounded, Enum, Read)
notestring = "ABCDEFG"            -- display string for Note

-- | A Pitch Octave following a Node, consiting of 1,2,3.
--   Octaves are in the Eq, Bounded, Enum and Read class.
data Octave = O1|O2|O3
    deriving (Eq, Bounded, Enum, Read)
octavestring = "123"              -- display string for Octave

-- | A Pitche consists of a Note and an Octave.
--   Pitches are in Eq class to determine if two Pitches match..
--   Pitches are shown as 'A1','G2', etc.  
data Pitch = Pitch {
     note :: Note
   , octave :: Octave }
        deriving (Eq)

-- | Defining display for Note, Octave and Pitch.
--   A Pitch dispay is a conbination of show Note and show Octave.
instance Show Note where
    show n = [notestring !! (fromEnum n)]
instance Show Octave where
    show o = [octavestring !! (fromEnum o)]
instance Show Pitch where
    show (Pitch n o) = show n ++ show o



{- codes below implements the composer part of the game,
 - including toPitch, feedback.
 -}
-- | toPitch is a composer function.
--   it gives Just the Pitch named by the string of user input,
--   or Nothing if string length is not 2, or
--   first char is not a Note string, or
--   second char is not a Octave string,
--   otherwise Just a coresponding pitch is returned.
toPitch :: String -> Maybe Pitch
toPitch xs
    | length xs /= 2 = Nothing
    | xsh `notElem` notestring = Nothing
    | xst `notElem` octavestring  = Nothing
    | otherwise = Just (Pitch (read [xsh] :: Note) (read ['O',xst] :: Octave))
        where xsh = head xs
              xst = last xs

-- | feedback is the composer function.
--   it takes the target chord and the performer's guess, and returns the feedback,
--   (correct Pitch numbers, correct Note numbers, correct Octave numbers).
--   correct Pitch numbers is the length of intersection between guess and target.
--   correct Note numbers is lenghth of Note intersection minus correct Pitch.
--   correct Octave numbers is length of Octave Intersection minus correct Pitch.
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (f1, f2, f3)
    where f1 = length (intersect guess target)
          f2 = length (intersect (map (note) guess) (map (note) target)) - f1
          f3 = length (intersect (map (octave) guess) (map (octave) target)) - f1



{- codes below implements the performer part of the game,
 - inlcuding GameState, initialGuess, nextGuess.
 -}
-- | GameState is a type of list of list of Pitches, 
--   which contains all conbinations of possible guess.
type GameState = [[Pitch]]

-- | initial Guess gives an initial guess to the composer,
--   including a hard coded first guess [A1,D2,G3],
--   and a full list of 1330 possible targets.
initialGuess :: ([Pitch], GameState)
initialGuess = ([(Pitch A O1), (Pitch D O2), (Pitch G O3)], (makeConbination pitchList 3))
-- | makeConbination  makes a conbination from a list
--   of the input numbers.
--   in our code it makes all 1330 possible targets from 21 Pitches.
makeConbination :: [a] -> Int ->[[a]]
makeConbination _ 0 = [[]]
makeConbination [] _ = []
makeConbination (x:xs) n = [x:ys | ys <- (makeConbination xs (n-1))]
    ++ (makeConbination xs n)
-- | pitchList lists all 21 Pitches by enermation over
--   all Notes and Octaves.
pitchList :: [Pitch]
pitchList = map makePitch [0..20]
    where makePitch n = Pitch (toEnum (n `div` 3)) (toEnum (n `mod` 3))

-- | nextGuess
--   takes previous guess and GameState, together with its feedback, 
--   to generate next guess and GameState.
--   based on feedback, inconsistant possible targets are removed
--   from GameState. Next guess follows the hints in the specification,
--   calulates the average expcted number of guess for each possible
--   target, and make the one with minimum value the next guess.
nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (guess, gamestate) (f1,f2,f3) = (calculatedGuess newgamestate, newgamestate)
    where newgamestate = removeByOctave (f1+f3) guess        -- remove inconsistent Octaves
                         (removeByNote (f1+f2) guess         -- remove inconsistent Notes
                         (removeByPitch f1 guess gamestate)) -- remove inconsistent Pitches
-- | removeByPitch removes inconsistent Pitches from GameState
--   by the no. of correct Pitches in feedback
removeByPitch :: Int -> [Pitch] -> GameState -> GameState
removeByPitch n guess gamestate =
    case n of
        0 -> filter (\a -> (length (intersect guess a)) == 0) gamestate
        1 -> filter (\a -> (length (intersect guess a)) == 1) gamestate
        2 -> filter (\a -> (length (intersect guess a)) == 2) gamestate
        3 -> filter (\a -> (length (intersect guess a)) == 3) gamestate
-- | removeByNote removes inconsistent Notes from GameState
--   by the no. of correct Notes in the feedback
removeByNote :: Int -> [Pitch] -> GameState -> GameState
removeByNote n guess gamestate = 
    case n of 
        0 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 0) gamestate
        1 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 1) gamestate
        2 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 2) gamestate
        3 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 3) gamestate
-- | removeByOctave removes inconsistent Octaves from GameState
--   by the no. of correct Octaves in the feedback
removeByOctave :: Int -> [Pitch] -> GameState -> GameState
removeByOctave n guess gamestate = 
    case n of  
        0 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 0) gamestate
        1 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 1) gamestate
        2 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 2) gamestate
        3 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 3) gamestate
--  | calculatedGuess takes the current GameState as input,
--    calculates the exptected avergae guesses left when taking
--    every remaining guess as target, and returns the minimum one.
calculatedGuess :: GameState -> [Pitch]
calculatedGuess gamestate =minExpPitch ([((getExpNum t gamestate),t) | t <- gamestate])
-- | minPitch takes the a list of 2-sized tuple as input, and
--   returns the second element of tuple who has the minimum
--   first element.
minExpPitch :: Ord a => [(a,b)] -> b
minExpPitch [] = error "minExpPitch on empty List"
minExpPitch (x:xs) = snd (minTail x xs)
    where minTail currentMin [] = currentMin
          minTail (m,n) (p:ps)
              | m > (fst p) = minTail p ps
              | otherwise   = minTail (m,n) ps
-- | getExpNum takes a target [Pitch] and GameState as input,
--   groups all possible targets in GameState with the same feedback,
--   retuns the average number of possible targets remains.
--   details please refer to hint 4&6 in the project specification.
getExpNum :: [Pitch] -> GameState -> Float
getExpNum target gamestate = 
    sum[(fromIntegral (length x))^2 | x <- (allFeedbacks target gamestate)]
        / fromIntegral (length gamestate)
            where allFeedbacks t g =
                      group (map (\a -> feedback t a) g)

-- ********** End of Code **********
