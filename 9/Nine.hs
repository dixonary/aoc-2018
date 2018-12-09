module Nine where

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
import qualified Data.Map  as Map
import qualified Data.List as List
import CTape
import qualified CTape


main = do
    -- Run parser and extract all data
    [players,marbles] <- runP readInts <$> readFile "input"

    -- Part 1
    print $ maximum $ scores players marbles

    -- Part 2
    print $ maximum $ scores players (marbles * 100)


-- Business logic for this day
scores :: Int -> Int -> [Int]
scores p n = CTape.toList $ play (singleton 0) p [1..n]

play :: CTape Int -> Int -> [Int] -> CTape Int
play circle players []          = CTape.fromList (replicate players 0)
play circle players (marble:ms) 
    | special marble = next 
                     $ add (marble + remMarble) 
                     $ play (delete circBack) players ms 
    | otherwise      = next 
                     $ play (insert marble $ next circle) players ms
    where
        special m = m `mod` 23 == 0
        circBack  = move (-7) circle 
        remMarble = CTape.elem circBack

add :: Int -> CTape Int -> CTape Int
add x (CTape r e s) = CTape r (e+x) s


-- Data loading apparatus
runP  :: ReadP a -> String -> a
runP parser str = readP_to_S parser str & last & fst

readInts :: ReadP [Int]
readInts = do
    let int    = read <$> munch1 isDigit 
        ignore = munch (not.isDigit)

    int `endBy` ignore
