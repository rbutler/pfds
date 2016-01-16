module Pfds.Ch05.SplayHeap where
import Prelude hiding (head, tail, last, init)
import Pfds.Ch03.Heap

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a)
  deriving (Eq, Show)

bigger pivot E = E
bigger pivot (T a x b)
  | x <= pivot = bigger pivot b
  | otherwise = case a of
    E -> T E x b
    T a1 y a2 ->
      if y <= pivot then
        T (bigger pivot a2) x b
      else
        T (bigger pivot a1) y (T a2 x b)
      
{-
insert x t = T a x b
  where (a,b) = partition x t
-}

partition pivot E = (E, E)
partition pivot t@(T a x b)
  | x <= pivot =
    case b of
      E -> (t, E)
      T b1 y b2 -> if y <= pivot then
          let (small, big) = partition pivot b2
          in (T (T a x b) y small, big)
        else
          let (small, big) = partition pivot b1
          in (T a x small, T big y b2)
  | otherwise =
    case a of 
      E -> (E, t)
      T a1 y a2 -> if y <= pivot then
        let (small, big) = partition pivot a2
        in (T a1 y small, T big x b)
      else
        let (small, big) = partition pivot a1
        in (small, T big y (T a2 x b))

instance Heap SplayHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  insert x t = T a x b
    where (a,b) = partition x t
  merge E t = t
  merge (T a x b) t = T (merge ta a) x (merge tb b)
    where (ta, tb) = partition x t

  findMin E = error "Empty heap"
  findMin (T E x b) = x
  findMin (T a x b) = findMin a
  
  deleteMin E = error "Empty heap"
  deleteMin (T E x b) = b
  deleteMin (T (T E x b) y c) = T b y c
  deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)

ex = foldr insert E [1..10]
ex2 = foldr insert E [10,9..1]

keanu = insert 1 (deleteMin ex) == ex
