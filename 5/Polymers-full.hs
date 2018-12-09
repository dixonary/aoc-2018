module Polymers where

import Data.Char (isDigit, ord, chr)

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
    string <- readFile "input"

    -- *** Part 1 ***
    print $ length $ simplify collapse string 

    -- *** Part 2 **
    let polys = [ simplify collapse str 
                | c <- ['A'..'Z']
                , let str = filter (`notElem` [c,chr $ ord c + 32]) string]
    print $ length $ minimumBy (compare `on` length) polys

collapse :: String -> String
collapse (a:b:x) = 
    let rest = collapse x 
    in if abs (ord a - ord b) == 32 then collapse x else a : collapse (b:x)
collapse x = x


collapse' :: [Int] -> [Int]
collapse' = foldl' addElem []
    where
        addElem soFar@(h:t) c = if c `elem` [h: 


converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

simplify f z = converge (==) $ iterate f z
