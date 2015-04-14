module Pfds.Ch05.Dequeue where
--import Prelude hiding (head, tail)

class Dequeue q where
  empty   :: q a
  isEmpty :: q a -> Bool

  cons :: a -> q a -> q a
  head :: q a -> a
  tail :: q a -> q a

  snoc :: q a -> a -> q a
  last :: q a -> a
  init :: q a -> q a

data BatchedDeque a = BD [a] [a]
  deriving (Show)

check [] r = BD (reverse r) []
check f r = BD f r

--instance Dequeue BatchedDeque where
  --empty = BD [] []
  --isEmpty (BD f r) = null f && null r

  -- Not correct yet
  --cons x (BD f r) = check (x:f) r
  --head (BD _ []) = error "Empty Queue"
  --head (BD f 

force a = seq (\a -> undefined) a 
  

