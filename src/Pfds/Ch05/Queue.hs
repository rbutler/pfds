module Pfds.Ch05.Queue where
import Prelude hiding (head, tail)

class Queue q where
  empty   :: q a
  isEmpty :: q a -> Bool
  snoc    :: q a -> a -> q a
  head    :: q a -> a
  tail    :: q a -> q a

data BatchedQueue a = BQ [a] [a]
  deriving (Show)

check [] r = BQ (reverse r) []
check f r = BQ f r

instance Queue BatchedQueue where
  empty = BQ [] []
  isEmpty (BQ f r) = null f
  
  snoc (BQ f r) x = check f (x:r)
  head (BQ [] _) = error "Empty Queue"
  head (BQ (x:f) r) = x
  tail (BQ [] _ ) = error "Empty Queue"
  tail (BQ (x:f) r) = check f r

q = snoc (BQ [] []) 1
q' = snoc q 2
q'' = snoc (snoc q' 3) 4
