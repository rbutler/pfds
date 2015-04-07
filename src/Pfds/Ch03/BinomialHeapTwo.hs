module Pfds.Ch03.BinomialHeap where
import Text.PrettyPrint
import Pfds.Ch03.Heap

-- Binomial Heaps (2.2)
data Tree a = Node a [Tree a]
  deriving (Show)
newtype BinomialHeap a = BH [(Int, Tree a)]
  deriving (Show)

--rank (Node r x c) = r
root (Node x c) = x

link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r+1) x1 (t2:c1)
  | otherwise = Node (r+1) x2 (t1:c2)

insTree t [] = [t]
insTree t ts@(t':ts')
  | rank t < rank t' = t:ts
  | otherwise        = insTree (link t t') ts'

mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2')
  | rank t1 < rank t2 = t1:(mrg ts1' ts2)
  | rank t2 < rank t1 = t2:(mrg ts1 ts2')
  | otherwise = insTree (link t1 t2) (mrg ts1' ts2')

removeMinTree [] = error "Empty Heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts)
  | root t < root t' = (t, ts)
  | otherwise        = (t', t:ts')
  where (t', ts') = removeMinTree ts

instance Heap BinomialHeap where
  empty = BH []
  isEmpty (BH ts) = null ts

  insert x (BH ts) = BH (insTree (Node 0 x []) ts)
  merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

  findMin (BH ts) = root t
    where (t, _) = removeMinTree ts

  deleteMin (BH ts) = BH (mrg (reverse ts1) ts2)
    where (Node _ x ts1, ts2) = removeMinTree ts

-- | Ex 3.5

findMin' (BH []) = error "Empty you bastards"
findMin' (BH [t]) = root t
findMin' (BH (t:ts)) = findMinWithCan (root t) ts
  where 
    findMinWithCan can [] = can
    findMinWithCan can [x]
      | can < root x = can
      | otherwise    = root x
    findMinWithCan can (x:xs)
      | can < root x = findMinWithCan can xs
      | otherwise    = findMinWithCan (root x) xs

-- | Ex 3.6

-- Test data

zero = Node 0 10 []
one = insTree (Node 0 9 []) [zero]

zero' = Node 0 11 []
one' = insTree (Node 0 12 []) [zero']

zero'' = Node 0 22 []

two = mrg one one'

test5 = BH (mrg two [zero''])

y = findMin test5
x = findMin' test5

