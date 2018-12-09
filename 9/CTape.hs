module CTape where

import Data.List hiding (insert)
import Data.Foldable hiding (toList)

data CTape a = CTape { 
    rprefix :: ![a], 
    elem :: !a, 
    suffix :: ![a] 
}
instance Show a => Show (CTape a) where
    show = intercalate " " . map show . toList

instance Foldable CTape where
    foldr f z = foldr f z . toList

-- Build a list from one element
singleton :: a -> CTape a
singleton e = CTape [] e []

fromList :: [a] -> CTape a
fromList [x]    = singleton x
fromList (x:xs) = insert x $ fromList xs

toList :: CTape a -> [a]
toList (CTape rp e ss) = reverse rp ++ [e] ++ ss

-- Insert new element to the right of current element
insert :: a -> CTape a -> CTape a
insert x (CTape rp e ss) = CTape (e : rp) x ss

-- Remove current element from the list
delete :: CTape a -> CTape a
delete (CTape rp _ (s:ss)) = CTape rp s ss
delete (CTape rp _ []    ) = CTape [] x xs      where (x:xs) = reverse rp

-- Move the head of the list by some amount (can be -ve) 
move 0 ct = ct
move n ct | n > 0     = move (n-1) $ next ct
          | otherwise = move (n+1) $ prev ct

next ct@(CTape [] e [])     = ct
next ct@(CTape rp e [])     = CTape [e] x xs    where (x:xs) = reverse rp
next ct@(CTape rp e (s:ss)) = CTape (e:rp) s ss

prev ct@(CTape [] e [])     = ct
prev ct@(CTape [] e ss)     = CTape xs x [e]    where (x:xs) = reverse ss
prev ct@(CTape (r:rp) e ss) = CTape rp r (e:ss)

