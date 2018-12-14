module Thirteen where

-- For reading data in nicely
import Text.ParserCombinators.ReadP 
import Data.Char 
import Data.Coerce
import Control.Monad
import Control.Arrow (first, second)
import Data.Function (on, (&))
import Data.List 
import Data.Functor ((<&>))
import Data.Ord
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import CTape


main = do
    -- Data loading
    let num = 554401

    -- Part 1

    -- Part 2
                            
split :: Int -> [Int]
split = map read . map (:[]) . show
