module Pfds.Ch03.RedBlack where
import Text.PrettyPrint
-- import Pfds.Ch03.Heap

data Color = R | B
  deriving (Show)
data RedBlackSet a = E | T Color (RedBlackSet a) a (RedBlackSet a)
  deriving (Show)


class Set s where
  empty  :: s a
  insert :: (Ord a) => a -> s a -> s a
  member :: (Ord a) => a -> s a -> Bool

balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

instance Set RedBlackSet where
  empty = E
  member x E = False
  member x (T _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True
  insert x s = T B a y b
    where tfoo@(T _ a y b) = ins s
          ins E = T R E x E
          ins s@(T color a y b)
            | x < y     = balance color (ins a) y b
            | x > y     = balance color a y (ins b)
            | otherwise = s

fromOrdList :: Ord x => [x] -> RedBlackSet x
fromOrdList [x] = insert x E
fromOrdList []  = E
fromOrdList (x:xs) = insert x (fromOrdList xs)

-- Example data
            
one = T B E 1 E
two = insert 3 $ one            
three = insert 13 $ two            
four = insert 7 $ three            
five = insert 12 $ four            



