module NineCoil where

-- A variant of Nine which uses a circular doubly-linked list.

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
import Coil
import qualified Coil
import CTape (CTape(..))
import qualified CTape

import Data.IORef


main = do
    -- Run parser and extract all data
    [players,marbles] <- runP readInts <$> readFile "input"

    s0 <- singleton 0

    -- Part 1
    scores <- play s0 players [1..marbles] 
    print $ maximum $ scores 

    -- Part 2
    scores <- play s0 players [1..marbles*100] 
    print $ maximum scores


-- Business logic for this day
play :: IORef (Coil Int) -> Int -> [Int] -> IO (CTape Int)
play cref ps []          = return $ CTape.fromList (replicate ps 0)
play cref ps (marble:ms) 

    | marble `mod` 23 == 0 = do
            circle   <- readIORef cref
            circBack <- move (-7) cref
            remValue <- val <$> readIORef circBack
            remCirc  <- delete circBack
            rest     <- play remCirc ps ms
            return $ CTape.next $ (marble + remValue) `add` rest

    | otherwise      = do
            newCirc  <- move 1 cref >>= insert marble
            rest     <- play newCirc ps ms
            return $ CTape.next $ rest


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
