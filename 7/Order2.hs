module Order where

-- For reading data in nicely
import Text.ParserCombinators.ReadP 
import Data.Char 
import Data.Coerce

import Control.Arrow (first, second)
import Data.Function (on, (&))
import Data.Ord
import Data.List 
import Data.Functor ((<&>))


main = do
    pairs <- readFile "input"
        <&> lines
        <&> map (read   :: String -> Pair     )
        <&> map (coerce :: Pair -> (Char,Char))

    let ready     ps   = filter (`notElem` map snd ps)
        removeDep ps c = filter ((/=c) . snd) 
                       $ filter ((/=c) . fst) ps


    -- Part 1
    let res ps chars  
            | chars == "" = ""
            | otherwise   = c : res ps' chars'
                where
                    (c:_)  = ready ps chars
                    ps'    = removeDep ps c
                    chars' = delete c chars
                  
    print $ res pairs ['A'..'Z'] 


    -- Part 2
    let tick :: Int               -- Tick
              -> [(Char, Int)]    -- Currently running workers
              -> Int              -- Free workers
              -> [Char]           -- Remaining Work
              -> [(Char,Char)]    -- Remaining Dependencies
              -> Int              -- Answer
        tick n workers free chars ps = 
            if done
                then n 
                else tick n' workers' free' chars' ps'
            where
                -- Find which workers are done, no time left.
                finWorkers = filter ((==0).snd) workers

                -- Remove all finished work from dependency lists.
                ps' = foldl' removeDep ps $ map fst finWorkers
        
                -- Find new work for any idle hands.
                readys   = sort $ ready ps' chars
                nowDoing = take (length finWorkers + free) readys
                
                -- Recalculate the number of free agents.
                free' = free + length finWorkers - length nowDoing

                -- Remove old workers and add new ones with times.
                newWorkers  = (workers \\ finWorkers) 
                            ++ map (\ch -> (ch, ord ch - 4)) nowDoing

                -- Remove any recently-begun work from the to-do.
                chars' = foldl' (flip delete) chars nowDoing

                -- Increase the time and tick the work down. 
                lowest   = minimum $ map snd newWorkers
                workers' = map (second $ subtract lowest) newWorkers
                n'       = n + lowest

                -- If there was no work left to do, we're finished!
                done = workers' == []


    print $ 
        tick 0                  -- Start at time 0
        (replicate 5 (' ',0))   -- 5 workers entering the pool
        0                       -- 0 pooled workers to begin with
        ['A'..'Z']              -- Work left
        pairs          
                            

-- *** Data types and marshalling functions ***

newtype Pair = Pair (Char, Char)

instance Read Pair where
    readsPrec _ = readP_to_S readRecord

readRecord :: ReadP Pair
readRecord = do
    optional $ string "Step "
    l <- get
    optional $ string " must be finished before step "
    r <- get
    optional $ string " can begin."
    return $ Pair (l,r)
