{-#LANGUAGE RecordWildCards#-}
module Twelve where

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
import Data.Bool

-- Useful data structures
import qualified Data.Map  as Map
import qualified Data.List as List

import Debug.Trace

main = do
    -- Run parser and extract all data
    Game st r <- runP readGame <$> readFile "input"
    
    let pad = 2000
        game' = Game (replicate pad False ++ st ++ replicate pad False) r

    -- Part 1
    let gs = iterate update game'

    let nums = zip (state $ gs !! 20) [-pad..]
    print $ sum $ map snd $ filter fst nums

    -- Part 2
    let findN n (x:y:xs) = let
            nextSt = tail $ state y 
            st     = state x
            diff   = filter (uncurry (/=)) $ zip nextSt st
            in if null diff then (n,x) else findN (n+1) (y:xs)

    let (conN, conS) = findN 0 gs
    let nums = zip (state conS) [-pad..]
    print $ sum $ map (+ (50000000000 - conN)) $ map snd $ filter fst nums


-- Business logic for this day
data Game = Game { state :: [Bool], rules :: [Rule] }
    deriving Eq
type Rule = ([Bool],Bool)


update :: Game -> Game
update Game{..} = Game state'' rules
    where
        state' = take 2 state ++ updatePot state
        state'' = take (length state' - 2) state'
        updatePot (a:b:c:d:e:xs) = 
            case lookup [a,b,c,d,e] rules of 
                Nothing -> error $ "missing rule:" ++ show [a,b,c,d,e]
                Just r  -> r : updatePot (b:c:d:e:xs)
        updatePot xs = xs


-- Data loading apparatus
runP  :: ReadP a -> String -> a
runP parser str = readP_to_S parser str & last & fst

readGame :: ReadP Game
readGame = do
    let isPot = (`elem` ['#','.'])
        ignore = munch1 (not.isPot)
        pot = (=='#') <$> satisfy isPot

    ignore
    state <- many pot

    ignore
    rules <- many $ do
            setup <- many pot
            ignore
            outcome <- pot
            ignore
            return (setup, outcome)

    return Game{..}
