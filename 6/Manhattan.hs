module Manhattan where

import Data.Char (isDigit, ord, chr)

import Control.Monad 
import Data.Ord (comparing)
import Data.Function (on)
import Control.Applicative

import Data.List

import Data.Time.Clock
import Data.Time.LocalTime 

main = do
    -- Data loading
    string <- liftM lines $ readFile "input"
    let coords = map (read.('(':).(++ ")")) string
        fsts   = map fst coords
        snds   = map snd coords

        nearest (x,y) = minimumBy (compare `on` manhattan (x,y)) coords 

    -- Part 1
    let
        allSpaces = (,) 
                    <$> [minimum fsts .. maximum fsts]
                    <*> [minimum snds .. maximum snds]

        allEdges = [ (x,y) 
                   | (x,y) <- allSpaces
                   ,  x == minimum fsts || y == minimum snds 
                   || x == maximum fsts || y == maximum snds
                   ]

    print $ modeFreq 
          $ filter (`notElem` map nearest allEdges) 
          $ map nearest allSpaces 

    -- Part 2
    print $ length [ (x,y) 
                   | (x,y) <- allSpaces
                   , sum (map (manhattan (x,y)) coords) < 10000
                   ]


-- Find the manhattan distance
manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (x,y) (cx, cy) = abs (cx - x) + abs (cy - y)

-- Get the number of appearances of each element in the list.
counts :: Eq a => [a] -> [(a,Int)]
counts xs = [ (e,count e xs) | e <- nub xs]
    where
        count x = length . filter (x==)

-- Find the element which occurs most frequently in the list.
mode :: Eq a => [a] -> a
mode list = fst $ maximumBy (comparing snd) $ counts list

-- Find the frequency of that mode.
modeFreq :: Eq a => [a] -> Int
modeFreq []   = 0
modeFreq list = snd $ maximumBy (comparing snd) $ counts list

