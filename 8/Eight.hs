module Eight where

-- For reading data in nicely
import Text.ParserCombinators.ReadP 
import Data.Char 
import Data.Coerce
import Control.Monad

import Control.Arrow (first, second)
import Data.Function (on, (&))
import Data.Ord
import Data.List 
import Data.Functor ((<&>))
import Data.Maybe

import Debug.Trace

main = do
    -- Data loading
    tree <- readFile "input"
         <&> init
         <&> (++" ")
         <&> (read :: String -> Tree)

    -- Part 1
    print $ tsum tree

    -- Part 2
    print $ rval tree
                            

-- *** Data types and marshalling functions ***
data Tree = Node [Tree] [Int]

tsum :: Tree -> Int
tsum (Node ts ms) = sum ms + (sum $ map tsum ts)

rval :: Tree -> Int
rval (Node [] ms) = sum ms
rval (Node ts ms) = ms
                  & filter (\x -> x > 0 && x <= length ts)
                  & map (subtract 1)
                  & map (ts !! )
                  & map rval
                  & sum


instance Read Tree where
    readsPrec _ = readP_to_S readTree

readTree :: ReadP Tree
readTree = do
    let int = liftM (read::String->Int) 
            $ manyTill (satisfy isDigit) (char ' ') 
    
    subnodes  <- int
    metadatas <- int
    
    subtrees <- replicateM subnodes  readTree
    metadata <- replicateM metadatas int
    
    return $ Node subtrees metadata


