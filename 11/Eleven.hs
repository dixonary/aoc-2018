module Eleven where

import Data.Function (on, (&))
import Data.Ord
import Data.List (maximumBy)

main = do
    let serial = 6878
    let rackSize = 300
        thd (a,b,c) = c

    -- Value of a cell
    let val x y = rackId 
                & ( * y )
                & ( + serial )
                & ( * rackId )
                & (`mod` 1000)
                & (`div` 100)
                & (subtract 5)
                where rackId = 10 + x

    -- Value of a window
    let wval size x y = sum 
          $ val <$> [x..x+size-1] <*> [y..y+size-1]

    -- Part 1
    print $ maximumBy (comparing thd) 
          $ [ (x,y,wval 3 x y) | x <- [1..rackSize-3], y <- [1..rackSize-3] ]

    -- Part 2

    -- (We assume that after -100, results only get worse)
    let ifl x y = [1..rackSize - max x y + 1] 
                & map (\n -> (n,wval n x y)) 
                & takeWhile ((> -100) . snd) 
                & maximumBy (comparing snd) 
                & (,,) x y

    print $ maximumBy (comparing (snd.thd)) 
          $ ifl <$> [1..rackSize] <*> [1..rackSize]
