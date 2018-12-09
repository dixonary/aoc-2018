{-# LANGUAGE RecursiveDo, BangPatterns #-}
module Coil where

import Data.IORef
import System.IO

data Coil a = Coil { 
    prev :: !(IORef (Coil a)), 
    val  :: !a, 
    next :: !(IORef (Coil a)) 
}

type Ref a = IO (IORef a)

instance Show a => Show (Coil a) where
    show = show . val

-- Build a list from one element
singleton :: a -> Ref (Coil a)
singleton e = mdo
    ref <- newIORef $ Coil ref e ref
    return ref

fromList :: [a] -> Ref (Coil a)
fromList [x]    = singleton x
fromList (x:xs) = insert x =<< fromList xs

-- Insert new element to the right of current element
insert :: a -> IORef (Coil a) -> Ref (Coil a)
insert e cref = do
    Coil pref v nref <- readIORef cref
    ref <- newIORef $ Coil cref e nref
    modifyIORef cref (\c -> c { next = ref }) 
    modifyIORef nref (\n -> n { prev = ref }) 
    return ref

-- Remove current element from the list
delete :: IORef (Coil a) -> Ref (Coil a)
delete cref = do
    Coil pref v nref <- readIORef cref
    modifyIORef pref (\p -> p { next = nref })
    modifyIORef nref (\n -> n { prev = pref })
    return nref

-- Move the head of the list by some amount (can be -ve) 
move :: Int -> IORef (Coil a) -> Ref (Coil a)
move 0 cref = return cref
move n cref = do 
    Coil pref v nref <- readIORef cref
    if n < 0 then move (n+1) pref else move (n-1) nref
