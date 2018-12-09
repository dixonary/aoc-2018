module Order where

import Text.ParserCombinators.ReadP 
import Data.Char 

import Control.Monad 
import Control.Arrow (second, first)
import Data.Function (on, (&))
import Data.Coerce
import Data.Functor
import Data.Ord (comparing)

import Data.Map (toList, insertWith, empty, Map)
import qualified Data.Map as Map
import Data.List (foldl', sort, delete, (\\))

import Data.Time.Clock
import Data.Time.LocalTime 

import Data.Ord
import Data.Coerce
import Data.Graph

import Data.Foldable (all,find)
import Debug.Trace
import Prelude hiding (all,find)


main = do
    pairs <- readFile "input"
        <&> lines
        <&> map (read   :: String -> Pair     )
        <&> map (coerce :: Pair -> (Char,Char))

    let depsWithoutNones :: Map Char [Char]
        depsWithoutNones = foldl' add empty pairs
            where add m (a,b) = insertWith (++) b [a] m

        deps = foldl' (\m x -> Map.insert x [] m) depsWithoutNones 
             $ filter (`notElem` map snd pairs) ['A'..'Z']

                    
    -- Part 1
    let resolve dmap 
            | dmap == empty = ""
            | otherwise     = 
                let
                    next  = minimum $ map fst $ toList $ Map.filter (==[]) dmap 
                    dmap' = dmap 
                          & Map.delete next         -- Remove next from the list of work
                          & Map.map (delete next)   -- Remove next from all dependencies
                in 
                    next : resolve dmap'          

    print $ resolve deps 


    -- Part 2
    let tick :: Int                -- Tick
              -> [(Char, Int)]      -- Currently running workers
              -> Int                -- Free workers
              -> Map Char [Char]    -- Remaining work, with dependencies
              -> Int                -- Answer
        tick n workers free remWork = 
            if done
                then n 
                else tick n' workers' free' remWork'
            where
                finWorkers = filter ((==0).snd) workers

                -- Remove all finished work from dependency lists.
                workLeft = foldl' (\m (c,0) -> Map.map (delete c) m) remWork finWorkers
        
                -- Find new work for any idle hands.
                readys = sort $ map fst $ toList $ Map.filter (==[]) workLeft
                nowDoing = take (length finWorkers + free) readys
                
                -- Recalculate the number of free agents.
                free' = free + length finWorkers - length nowDoing

                -- Remove old workers and add new ones with times.
                withTime ch = (ch, ord ch - 4)
                workers' = (workers \\ finWorkers) ++ map withTime nowDoing
                         & map (second $ subtract 1)

                -- Remove any recently-begun work from the to-do.
                remWork' = foldl' (\m c -> Map.delete c m) workLeft nowDoing

                -- If there was no work left to do, we're finished!
                done = workers' == []

                -- Increase the tick
                n' = n + 1

    print $ 
        tick 0                  -- Start at time 0
        (replicate 5 (' ',0))   -- Initialise with 5 workers entering the pool
        0                       -- 0 pooled workers to begin with
        deps          
                            

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
