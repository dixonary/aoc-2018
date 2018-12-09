module Main where

-- Data loading
import Text.ParserCombinators.ReadP 
import Data.Char 
import Data.Coerce

-- Useful functions
import Control.Monad 
import Control.Arrow (second, first)
import Data.Function (on, (&))
import Data.Functor
import Data.Foldable
import Data.Ord

-- Useful data structures
import qualified Data.Map as Map
import Data.List.PointedList.Circular       -- library PointedList
import Data.List.PointedList (singleton)    -- library PointedList

import Debug.Trace


main = do
    -- Run parser and extract all data
    [players,marbles] <- runP readInts <$> readFile "input"

    let
        play :: PointedList Int -> [Int] -> [Int] 
        play circle []          = []
        play circle (marble:ms) 
            | special marble = marble + remMarble : play (delete' circBack) ms
            | otherwise      = 0 : play (circle & next & insert marble) ms

            where
                special m = m `mod` 23 == 0
                circBack  = circle & moveN (-7) 
                remMarble = _focus circBack
                delete' l = let Just l' = delete l in l'

        game :: Int -> [Int]
        game n = play (singleton 0) [1..n]

        scores :: Int -> Int -> [Int]
        scores p m = map snd $ combine $ zip (cycle [1..p]) $ game m

    -- Part 1
    print $ maximum $ scores players marbles

    -- Part 2
    print $ maximum $ scores players (marbles * 100)


-- Useful bonus functions on lists
-- Take an association list and sum up the second element,
-- grouped by the first
combine :: (Eq a, Ord a, Num b) => [(a,b)] -> [(a,b)]
combine = Map.toList . foldl' add Map.empty
    where add map (k,x) = Map.insertWith (+) k x map


-- Data loading apparatus
runP  :: ReadP a -> String -> a
runP parser str = readP_to_S parser str & last & fst

readInts :: ReadP [Int]
readInts = do
    let ignore = munch (not.isDigit)
        int    = read <$> munch1 (isDigit)

    int `endBy` ignore
