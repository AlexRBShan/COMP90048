{- /
Module		: Proj1
Description	: Project 1 of COMP90048 in 19S1.
Copyright	: Rongbing Shan 945388 @Unimelb
DateCreated	: APR 4th, 2019:

Maintainer	: alex.shan@student.unimelb.edu.au

<module description>
-}
--module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where
import Data.List

-- | A Pitch Node consisting of A,B, ..,G.
--   Notes are ordered alphabetically.
data Note = A|B|C|D|E|F|G
    deriving (Eq, Ord, Bounded, Enum, Read)
notestring = "ABCDEFG"

-- | A Pitch Octave following a Node, consiting of 1,2,3.
--   Octaves are ordered algebraically.
data Octave = O1|O2|O3
    deriving (Eq, Ord, Bounded, Enum, Read)
octavestring = "123"

-- | A Pitche consists of a Note and an Octave.
--   Pitches are in Ord Class, ordered by primarily Note and secondly by Octave.
--   Pitches are in Bounded and Enum, to enumerate all Pitches.
--   Pitches are enumerated in the same way as order is defined.
--   Pitches are shown as 'A1','G2', etc.  
data Pitch = Pitch {
     note :: Note
   , octave :: Octave }
        deriving (Eq, Bounded)
-- | Define order of Pitches.
instance Ord Pitch where
    compare (Pitch n1 o1) (Pitch n2 o2) =
        if noteorder == EQ then compare o1 o2 else noteorder
            where noteorder = compare n1 n2
-- | Define enumeration of Pitches.
instance Enum Pitch where
    fromEnum (Pitch n o) = (fromEnum n) * 3 + (fromEnum o)
    toEnum i = (Pitch n o)
        where n = toEnum (i `div` 3)
              o = toEnum (i `mod` 3)
-- | Defining display for Note, Octave and Pitch.
instance Show Note where
    show n = [notestring !! (fromEnum n)]
instance Show Octave where
    show o = [octavestring !! (fromEnum o)]
instance Show Pitch where
    show (Pitch n o) = show n ++ show o


-- | toPitch
--   gives Just the Pitch named by the string,
--   or Nothing if the string is not a valid pitch name.
toPitch :: String -> Maybe Pitch
toPitch xs
    | length xs /= 2 = Nothing
    | xsh `notElem` notestring = Nothing
    | xst `notElem` octavestring  = Nothing
    | otherwise = Just (Pitch (read [xsh] :: Note) (read ['O',xst] :: Octave))
        where xsh = head xs
              xst = last xs

-- | feedback
--   takes the target chord and the guess,
--   and returns the feedback.
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (f1, f2, f3)
    where f1 = length (intersect target guess)
          f2 = length (intersect (map (note) target) (map (note) guess)) - f1
          f3 = length (intersect (map (octave) target) (map (octave) guess)) - f1
{-
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback _ [] = error "error: invalid guess detected"
feedback [] _ = error "error: invalid target detected"
feebback t g = ((correctPitch t g), (correctNote t g), (correctOctave t g))

correctPitch :: [Pitch] -> [Pitch] -> Int
correctPitch t [] = 0
correctPitch t (g:gs) = 
    if g `elem` t then 1 + correctPitch t gs
        else correctPitch t gs
correctNote :: [Pitch] -> [Pitch] -> Int
correctNote t g = length (filter (`elem` tNote) gNote)
    where tNote = map (note) (filter (`notElem` g) t)
          gNote = map (note) (filter (`notElem` t) g)
correctOctave :: [Pitch] -> [Pitch] -> Int
correctOctave t g = length (filter (`elem` tOctave) gOctave)
    where tOctave = map (octave) (filter (`notElem` g) t)
          gOctave = map (octave) (filter (`notElem` t) g)
-}

-- | Gamestate to hold possible targets
--
type GameState = [[Pitch]]
-- | initial Guess initialize a hard coded 
--   initial guess and start game state.
initialGuess :: ([Pitch], GameState)
initialGuess = ([(Pitch A O1), (Pitch B O1), (Pitch C O2)], (makeConbination pitchList 3))


makePitch :: Int -> Pitch
makePitch n = Pitch (toEnum (n `div` 3)) (toEnum (n `mod` 3))
pitchList :: [Pitch]
pitchList = map makePitch [0..20]


-- | makeConbination  makes a conbination from a list
--   of the input numbers
makeConbination :: [a] -> Int ->[[a]]
makeConbination _ 0 = [[]]
makeConbination [] _ = []
makeConbination (x:xs) n = [x:ys | ys <- (makeConbination xs (n-1))]
    ++ (makeConbination xs n)

-- | nextGuess
--   takes previous guess and game state, together with its feedback, 
--   to generate next guess and game state.
nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (guess, gamestate) (f1,f2,f3) = (calculatedGuess newgamestate, newgamestate)
    where newgamestate = removeByOctave (f1+f3) guess (removeByNote (f1+f2) guess (removeByPitch f1 guess gamestate))

-- | removeByPitch removes incorrect possible guesses from game state
--   by the no. of correct Pitches in feedback
removeByPitch :: Int -> [Pitch] -> GameState -> GameState
removeByPitch n guess gamestate =
    case n of
        0 -> filter (\a -> (length (intersect guess a)) == 0) gamestate
        1 -> filter (\a -> (length (intersect guess a)) == 1) gamestate
        2 -> filter (\a -> (length (intersect guess a)) == 2) gamestate
        3 -> filter (\a -> (length (intersect guess a)) == 3) gamestate
-- | removeByNote removes incorrect possible guesses from game state
--   by the no. of correct Notes in the feedback
removeByNote :: Int -> [Pitch] -> GameState -> GameState
removeByNote n guess gamestate = 
    case n of 
        0 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 0) gamestate
        1 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 1) gamestate
        2 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 2) gamestate
        3 -> filter (\a -> (length (intersect (map (note) guess) (map (note) a))) == 3) gamestate
-- | removeByOctave removes incorrect possible guesses from game state
--   by the no. of correct Octaves in the feedback
removeByOctave :: Int -> [Pitch] -> GameState -> GameState
removeByOctave n guess gamestate = 
    case n of  
        0 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 0) gamestate
        1 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 1) gamestate
        2 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 2) gamestate
        3 -> filter (\a -> (length (intersect (map (octave) guess) (map (octave) a))) == 3) gamestate

--  | calculatedGuess takes the current game state as input,
--    calculates the exptected avergae guesses left when taking
--    every remaining guess as target, and returns the minimum one
calculatedGuess :: GameState -> [Pitch]
calculatedGuess gamestate =snd(minimum( [((getExpNum t gamestate),t) | t <- gamestate]))

-- | allfeedbacks makes a list of feedbacks
--   by taking every possbile remaining guess as the target
allFeedbacks :: [Pitch] -> GameState -> [(Int, Int, Int)]
allFeedbacks target gamestate = map (\a -> feedback target a) gamestate
-- |
--
getExpNum :: [Pitch] -> GameState -> Float
getExpNum guess target = 
    sum[(fromIntegral (length x)) * (fromIntegral(length x)) | x <- xs]
        / fromIntegral (length target)
            where xs = group (allFeedbacks guess target)

-- testing purpose
t1 = [(Pitch D O1),(Pitch B O1),(Pitch G O2)] -- D1 B1 G2
p1 = [(Pitch A O1),(Pitch B O1),(Pitch C O2)] -- A1 B1 C2
p2 = [(Pitch A O1),(Pitch E O3),(Pitch G O2)] -- A1 E3 G2
p3 = [(Pitch A O1),(Pitch D O2),(Pitch F O3)] -- A1 D2 F3
p4 = [(Pitch B O1),(Pitch D O3),(Pitch G O2)] -- B1 D3 G2 



