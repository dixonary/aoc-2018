{-# LANGUAGE RecordWildCards #-}

module Intersects where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP hiding (get)
import Control.Monad (liftM)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Map (Map, fromList, toList, 
                 alter, empty, insertWith)
import qualified Data.Map as Map


main = do
    boxes <- liftM (map read . lines) $ getContents
          :: IO [Box]

    -- *** Part 1 ***
    let grid = foldl' insertBox empty boxes

    print $ foldl' (+) 0 
          $ Map.map (length . (Map.filter (>=2))) 
            grid

    -- *** Part 2 ***
    print $ head 
          [boxId box
          | box <- boxes
          ,  not $ any (>=2) $ map (get grid) $ allInches box
          ]
                

type Grid = Map Int (Map Int Int)

-- Put a box into the grid.
insertBox :: Grid -> Box -> Grid
insertBox grid box = foldl' (flip increment) 
                     grid (allInches box)

-- Add 1 to the number of occupiers.
increment :: (Int, Int) -> Grid -> Grid
increment (x,y) = insertWith increment' x empty
    where 
        increment' _ = insertWith (+) y 1

-- Get the value at a position in the grid
get :: Grid -> (Int, Int) -> Int
get grid (x,y) = fromMaybe 0 $ Map.lookup x grid >>= Map.lookup y 



data Box = Box {
    boxId   :: Int,
    top     :: Int,
    left    :: Int,
    width   :: Int,
    height  :: Int
} deriving (Show)

allInches :: Box -> [(Int,Int)]
allInches Box{..} = [ (x,y) 
                    | x <- [left .. left + width  - 1]
                    , y <- [top  .. top  + height - 1] 
                    ]

instance Read Box where
    readsPrec _ = readP_to_S readBox

readBox :: ReadP Box
readBox = do
    let number = many1 $ satisfy isDigit
        ignore = optional . string

    ignore "#"
    boxId <- read <$> number

    ignore " @ "
    [left,top] <- (map read) <$> number `sepBy` (char ',')

    ignore ": " 
    [width,height] <- (map read) <$> number `sepBy` (char 'x')
    
    return $ Box {..}
    
