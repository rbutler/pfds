module Pfds.Ch05.Dequeue where
import Prelude hiding (head, tail, last, init)
import Pfds.Ch03.Heap

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a)
  deriving (Eq)

--partition pivot E = (E, E)
--partition pivot t@(T a x b)
  --| x <= pivot = 


