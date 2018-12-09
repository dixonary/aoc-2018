module Checksum where

import Control.Monad (liftM)
import Data.List (group, sort)

main = do
    lines <- liftM lines $ getContents
    let counts = map (map length . group . sort) lines
    
    -- Part 1
    print $ (length . filter (3 `elem`)) counts
          * (length . filter (2 `elem`)) counts

    -- Part 2
    let conzip f a b = concat $ zipWith f a b
    let sim  = conzip (\a b -> if a==b then [a] else [] )
    let diff = conzip (\a b -> if a==b then []  else [a])

    putStrLn $ head
        [ sim a b
        | a <- lines, 
          b <- lines, 
          length (diff a b) == 1
        ]
