module Listy where

import Control.Monad (liftM)
import System.IO (isEOF)

main = do
    changes <- changeList 

    -- Part 1
    print $ sum changes

    -- Part 2, takes a couple mins
    let allValues = scanl (+) 0 (cycle changes)
    print $ firstRepeat allValues
    

-- Finds the first element which appears in the list for a second time.
-- The list need not be finite.
firstRepeat :: Eq a => [a] -> Maybe a
firstRepeat xs = firstRepeat' xs [] 
    where
        firstRepeat' [] _ = Nothing
        firstRepeat' (x:xs) prevs =
            if x `elem` prevs
                then Just x
                else firstRepeat' xs (x:prevs)


-- Read in all the changes from stdin
changeList :: IO [Int]
changeList = do
    eof <- isEOF
    if eof 
        then return []
        else do
            (h:t) <- getLine
            -- Get rid of the + since +5 is not a number but -5 is
            let change = case h of 
                    '+' -> read t
                    _   -> read (h:t)
            liftM (change :) changeList
