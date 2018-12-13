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


main = do
    -- Data loading
    track <- readFile "input"
         <&> (readTrack)

    -- Part 1
    print $ untilLeft update track

    -- Part 2
    print $ untilLeft updateAndRemove track
                            
untilLeft :: (a -> Either e a) -> a -> e
untilLeft f x = do
    let x' = f x
    case x' of
        Left l -> l
        Right r -> untilLeft f r

update :: Track -> Either (Int,Int) Track
update t@(Track railmap carts) = Track railmap <$> updateCarts [] carts
    where 
        updateCarts :: [CartMeta] -> [CartMeta] -> Either (Int,Int) [CartMeta]
        updateCarts dones [] = Right dones
        updateCarts dones (((y,x),dir,intention):cs) = 
            if collision 
                then Left (x',y') -- Collision point 
                else updateCarts (((y',x'),dir',intention'):dones) cs
                where
                    ((y',x'),dir',intention') = move ((y,x),dir,intention) railmap
                    collides ((a,b),_,_) = a == y' && b == x'
                    collision = any collides $ dones ++ cs
                

updateAndRemove :: Track -> Either (Int,Int) Track
updateAndRemove t@(Track railmap carts) = Track railmap <$>  updateCarts [] carts
    where 
        updateCarts :: [CartMeta] -> [CartMeta] -> Either (Int,Int) [CartMeta]
        updateCarts dones [] = case dones of
            [((y',x'),dir',intention')] -> Left (x',y')
            _                           -> Right $ sortBy (comparing (\(l,a,b) -> l)) dones
                
        updateCarts dones (((y,x),dir,intention):cs) = 
            updateCarts dones' cs'
            where
                ((y',x'),dir',intention') = move ((y,x),dir,intention) railmap
                collides ((a,b),_,_) = a == y' && b == x'
                collision = any collides $ dones ++ cs

                dones' = if collision then filter (not.collides) dones else ((y',x'),dir',intention') : dones
                cs'    = if collision then filter (not.collides) cs    else                             cs
                    

-- Move a cart one step along the track.
move :: CartMeta -> Railmap -> CartMeta
move ((y,x),dir,intention) railmap = ((y',x'),dir',intention') 
    where
        (y',x') = case dir of
            N -> (y-1,x)
            S -> (y+1,x)
            E -> (y,x+1)
            W -> (y,x-1)

        Just newLoc = get2d x' y' railmap 

        dir' = case newLoc of
            Vert -> dir
            Horz -> dir
            NE -> if dir == W then N else E
            NW -> if dir == E then N else W
            SE -> if dir == W then S else E
            SW -> if dir == E then S else W
            Intn -> takeJunction intention dir
            None -> error $ "How did I get here?\n"
                            ++ show (y,x,dir,intention) ++ "\n"
        
        intention' = case newLoc of
                     Intn -> next intention
                     _    -> intention 
        

-- A two-dimensional grid
type Map2D a = Map Int (Map Int a)

ins2d :: Int -> Int -> a -> Map2D a -> Map2D a
ins2d x y r rm = Map.alter (\m -> case m of
        Nothing -> Just $ Map.insert x r Map.empty
        Just m' -> Just $ Map.insert x r m') y rm

fmap2d :: (a->b) -> Map2D a -> Map2D b
fmap2d = Map.map . Map.map 

get2d :: Int -> Int -> Map2D a -> Maybe a
get2d x y m = Map.lookup x =<< Map.lookup y m 



-- *** Data types and marshalling functions ***
data Track = Track Railmap [CartMeta]

type CartMeta = ((Int,Int), Cart, Intention)

type Railmap = Map2D Rails

data Cart  = N | S | E | W
    deriving (Eq)
data Rails = None | Intn | Vert | Horz | NE | NW | SE | SW
    deriving (Eq)
data Intention = L | St | R
    deriving (Eq,Show)

takeJunction :: Intention -> Cart -> Cart
takeJunction L S = E
takeJunction L E = N
takeJunction L N = W
takeJunction L W = S
takeJunction R S = W
takeJunction R E = S
takeJunction R N = E
takeJunction R W = N
takeJunction St S = S
takeJunction St E = E
takeJunction St N = N
takeJunction St W = W

next :: Intention -> Intention
next L = St
next St = R
next R = L

instance Show Track where
    show (Track railmap carts) = do
        let strMap = fmap2d show railmap
        let ins ((y,x),c,i) = ins2d x y (show c)
        let strCartMap = foldr ins strMap carts
        let str = Map.elems . Map.map (concat . Map.elems) $ strCartMap 

        unlines str

instance Show Cart where
    show N = "^"
    show S = "v"
    show E = ">"
    show W = "<"

instance Show Rails where
    show None = " "
    show Intn = "+"
    show Vert = "|"
    show Horz = "-"
    show NE   = "\\"
    show NW   = "/"
    show SE   = "/"
    show SW   = "\\"


readTrack :: String -> Track
readTrack str = do
    let indexed = zip [0..] $ map (zip [0..]) $ lines str
    let uncombine :: (a,[(b,c)]) -> [(b,a,c)]
        uncombine (a,xs) = map (\(b,c) -> (b,a,c)) xs
        icomb = concatMap uncombine indexed 
        newRailmap = foldl' f (Track Map.empty []) icomb
        f :: Track -> (Int,Int,Char) -> Track
        f (Track railmap carts) (x,y,c) = case c of
            '^' -> Track (ins2d x y Vert railmap) (((y,x),N,L):carts) 
            'v' -> Track (ins2d x y Vert railmap) (((y,x),S,L):carts) 
            '>' -> Track (ins2d x y Horz railmap) (((y,x),E,L):carts) 
            '<' -> Track (ins2d x y Horz railmap) (((y,x),W,L):carts) 
            ' ' -> Track (ins2d x y None railmap) carts
            '+' -> Track (ins2d x y Intn railmap) carts
            '|' -> Track (ins2d x y Vert railmap) carts
            '-' -> Track (ins2d x y Horz railmap) carts
            '\\' -> let dir = if get2d x (y-1) railmap `elem` [Just Intn, Just Vert, Just SW, Just SE]
                            then NE else SW
                            in Track (ins2d x y dir railmap) carts
            '/' -> let dir = if get2d x (y-1) railmap `elem` [Just Intn, Just Vert, Just SW, Just SE]
                            then NW else SE
                            in Track (ins2d x y dir railmap) carts

    newRailmap
