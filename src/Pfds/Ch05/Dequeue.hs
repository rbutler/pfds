module Pfds.Ch05.Dequeue where
import Prelude hiding (head, tail, last, init)

class Dequeue q where
  empty   :: q a
  isEmpty :: q a -> Bool

  cons :: a -> q a -> q a
  head :: q a -> a
  tail :: q a -> q a

  snoc :: q a -> a -> q a
  last :: q a -> a
  init :: q a -> q a

data BatchedDequeue a = BD [a] [a]
  deriving (Show)

split :: [a] -> ([a], [a])
split l = splitAt (((length l) + 1) `div` 2) l

check [] [] = BD [] []
check [] r = BD (reverse b) a
  where (a, b) = split r
--check f [] = BD b (reverse a)
check f [] = BD a (reverse b)
  where (a, b) = split f
check f r = BD f r

instance Dequeue BatchedDequeue where
  empty = BD [] []
  isEmpty (BD f r) = null f && null r

  cons x (BD f r) = check (x:f) r

  head (BD [] []) = error "Empty Queue"
  head (BD [] [x]) = x
  head (BD (x:f) r) = x

  tail (BD [] []) = error "Empty Queue"
  tail (BD [] [x]) = BD [] []
  tail (BD (x:f) r) = check f r


  snoc (BD f r) x = check f (x:r)

  last (BD [] []) = error "Empty Queue"
  last (BD [x] []) = x
  last (BD f (x:r)) = x

  init (BD [] []) = error "Empty Queue"
  init (BD (x:r) [])  = BD [] r
  init (BD f (x:r)) = check f r

--force a = seq (\a -> undefined) a 
  

q = snoc (BD [] []) 1
q' = snoc q 2
q'' = snoc (snoc q' 3) 4
who = snoc ( tail $ tail q'' ) 13
whoa = snoc (snoc (snoc ( tail $ tail q'' ) 13) 15) 16
