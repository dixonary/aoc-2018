{-#LANGUAGE RecordWildCards #-}
module Nine where

import Text.ParserCombinators.ReadP 
import Data.Char 
import Data.Function (on, (&))
import Data.Functor ((<&>))

main = do
    -- Run parser and extract all data
    items <-  readFile "input"
            <&> lines
            <&> map (runP readItem)

    let yrange (t,items) = maximum (map y items) - minimum (map y items)
        inflection st    = yrange (update st) > yrange st

    let res = until inflection update (0,items)

    -- Part 1
    mapM_ putStrLn $ printGrid $ snd res

    -- Part 2
    print $ fst res


-- Business logic for this day
update :: (Int,[Item]) -> (Int,[Item])
update (t,is) = (t+1,map update' is)
    where update' Item{..} = Item (x+vx) (y+vy) vx vy

exists :: (Int,Int) -> [Item] -> Bool
exists (x',y') = any (\(Item{..}) -> x' == x && y' == y)

printGrid :: [Item] -> [String]
printGrid items = 
    [ [ if exists (x',y') items then '*' else ' '
        | x' <- [minimum (map x items) .. maximum(map x items)] ] 
        | y' <- [minimum (map y items) .. maximum(map y items)] ]


data Item = Item { x :: Int, y :: Int, vx :: Int, vy :: Int }
    deriving (Show, Eq)

readItem :: ReadP Item
readItem = do
    let isNum x = isDigit x || x == '-'
    munch $ not.isNum
    [x,y,vx,vy] <- readInts
    return $ Item x y vx vy


-- Data loading apparatus
runP  :: ReadP a -> String -> a
runP parser str = readP_to_S parser str & last & fst
    
readInts :: ReadP [Int]
readInts = do
    let isNum x = isDigit x || x == '-'
        int     = read <$> munch1 isNum
        ignore  = munch $ not.isNum

    int `endBy` ignore
