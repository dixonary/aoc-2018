module Guards where

import Text.ParserCombinators.ReadP 
import Data.Char (isDigit)

import Control.Monad 
import Control.Arrow (second)
import Data.Function (on, (&))
import Data.Ord (comparing)
import Data.Coerce

import Data.Map (toList, insertWith, empty)
import Data.List

import Data.Time.Clock
import Data.Time.LocalTime 
 
import Debug.Trace


main = do
    -- Data loading
    lines <- liftM lines $ readFile "input"
    
    let 
        -- Combine into one item for each day
        collate :: [(Int,[Int])] -> Int -> [(Int,[Int])]
        collate recs m 
            | m > 60    = (m,[]):recs
            | otherwise = second (++[m]) (head recs) : tail recs

        -- Convert list of times to sleep/wake intervals
        intervals :: [Int] -> [Int]
        intervals (s:w:xs) = [s..w-1] ++ intervals xs
        intervals xs       = xs
        
    let records = lines
            & sort
            & map (read :: String -> Record)
            & map (coerce :: Record -> Int) 
            & foldl' collate []
            & map (second intervals)
            & combineLists

    -- *** Part 1 ***
    let mostAsleep = maximumBy (comparing (length.snd)) records
    print $ (fst mostAsleep) * (mode $ snd mostAsleep)

    -- *** Part 2 **
    let modallyAsleep = maximumBy (comparing (modeFreq.snd)) records
    print $ (fst modallyAsleep) * (mode $ snd modallyAsleep)
    


-- *** List functions ***

-- Group by the first element and pull it out to form an association list.
combineLists :: (Eq a, Ord a) => [(a,[b])] -> [(a,[b])]
combineLists = toList . (foldl' add empty)
    where 
        add map (k,x) = insertWith (++) k x map
    
-- Get the number of appearances of each element in the list.
counts :: Eq a => [a] -> [(a,Int)]
counts xs = [ (e,count e xs) | e <- nub xs]
    where
        count x = length . filter (x==)

-- Find the element which occurs most frequently in the int list.
mode :: Eq a => [a] -> a
mode list = fst $ maximumBy (comparing snd) $ counts list

-- Find the frequency of that mode.
modeFreq :: Eq a => [a] -> Int
modeFreq []   = 0
modeFreq list = snd $ maximumBy (comparing snd) $ counts list



-- *** Data types and marshalling functions ***

newtype Record = Record Int
instance Read Record where
    readsPrec _ = readP_to_S readRecord

readRecord :: ReadP Record
readRecord = do
    let int = liftM read $ many1 $ satisfy isDigit
        ignore = optional . string

    mins <- between (manyTill get (char ':')) (char ']') int

    let guard = do 
            ignore " Guard #"
            guardId <- int
            ignore " begins shift"
            return . (coerce :: Int -> Record) $ guardId
    let event = do
            manyTill get eof
            return . (coerce :: Int -> Record) $ mins

    guard <++ event

