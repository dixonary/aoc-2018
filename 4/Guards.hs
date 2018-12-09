module Guards where

import Text.ParserCombinators.ReadP 
import Data.Char (isDigit)

import Control.Monad 
import Control.Arrow (second)
import Data.Function (on)
import Data.Ord (comparing)

import Data.Map (toList, insertWith, empty)
import Data.List

import Data.Time.Clock
import Data.Time.LocalTime 


main = do
    -- Data loading
    records <- liftM (map read . sort . lines) 
            $ readFile "input"
            :: IO [Record]

    -- Combine into one element for each day
    let dayRecords :: [(Int, [Record])]
        dayRecords = foldl' collate [] records
            where
                collate recs           (Guard g t) = (g, []  ) : recs
                collate ((g, rs):rest) r           = (g, r:rs) : rest

    -- Combine all sleep intervals from all days, for each guard
    let guardRecords = combineLists dayRecords

    -- Convert from sleep records to time intervals
    let guardSleepMinutes = map recordToMins guardRecords
            where
                recordToMins = second $ concatMap mins . intervals

                -- Pair up the sleep and wake times
                intervals recs = zip 
                    (map tod $ filter isSleep recs) 
                    (map tod $ filter isWake recs)
                
                -- Convert from a time interval to a set of minutes 
                mins (begin,end) = [todMin begin .. todMin end - 1]


    -- *** Part 1 ***
    let mostAsleep = maximumBy (comparing (length.snd)) 
                        $ guardSleepMinutes

    print $ (fst mostAsleep) * (mode $ snd mostAsleep)


    -- *** Part 2 **
    let modallyAsleep = maximumBy (comparing (modeFreq.snd))
                        $ guardSleepMinutes

    print $ (fst modallyAsleep) * (mode $ snd modallyAsleep)
    


-- *** List functions ***

-- Group by the first element and pull it out to form an association list.
combine :: (Eq a, Ord a) => [(a,b)] -> [(a,[b])]
combine = toList . (foldl' add empty)
    where 
        add map (k,x) = insertWith (++) k [x] map

-- As above, but it concatenates lists instead of elements
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

data Record = Guard Int -- Guard ID
            | Sleep Int -- Time
            | WakeUp Int -- Time
            deriving (Show)

instance Eq Record where
    (==) = (==) `on` time

isWake :: Record -> Bool
isWake (WakeUp _) = True
isWake _          = False

isSleep :: Record -> Bool
isSleep (Sleep _) = True
isSleep _         = False

time :: Record -> UTCTime
time (Guard _ t) = error "Not needed" 
time (WakeUp  t) = t 
time (Sleep   t) = t  

tod :: Record -> TimeOfDay
tod = localTimeOfDay . (utcToLocalTime utc) . time



instance Read Record where
    readsPrec _ = readP_to_S readRecord

readRecord :: ReadP Record
readRecord = do
    let int = liftM read $ many1 $ satisfy isDigit
        ignore = optional . string

    time <- liftM (read . (++ ":00 UTC")) 
            $ between (char '[') (char ']') 
            $ many get 

    choice 
        [ do 
            ignore " Guard #"
            guardId <- int
            ignore " begins shift"
            return $ Guard guardId time
        , do
            ignore " falls asleep"
            return $ Sleep time
        , do
            ignore " wakes up"
            return $ WakeUp time
        ]

