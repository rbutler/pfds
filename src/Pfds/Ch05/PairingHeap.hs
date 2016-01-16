module Pfds.Ch05.PairingHeap where
import Prelude hiding (head, tail, last, init)
import Pfds.Ch03.Heap

data PairingHeap a = E | T a [PairingHeap a]
  deriving (Show)

instance Heap PairingHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  insert x h = merge (T x []) h

  merge h E = h
  merge E h = h
  merge h1@(T x hs1) h2@(T y hs2)
    | x < y = T x (h2:hs1)
    | otherwise = T y (h1:hs2)

  findMin E = error "Empty heap"
  findMin (T x hs) = x

  deleteMin E = error "Empty heap"
  deleteMin (T x hs) = mergePairs hs
    where
      mergePairs [] = E
      mergePairs [h] = h
      mergePairs (h1:h2:hs') = merge (merge h1 h2) (mergePairs hs')
    
ex = foldr insert E [1..5]
ex2 = foldr insert ex [10..15]
ex3 = foldr insert ex2 [6..9]
