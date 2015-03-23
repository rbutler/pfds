{-# LANGUAGE TypeSynonymInstances #-}
module Ch2 where

import Debug.Trace
import Control.Monad (liftM)
import Control.Monad.Trans.Class

-- Figure 2.1
class Stack s where
  empty   :: s a
  isEmpty :: s a -> Bool
  cons    :: a -> s a -> s a
  head    :: s a -> a
  tail    :: s a -> s a

-- Figure 2.2
instance Stack [] where
  empty = []
  isEmpty [] = True
  isEmpty _ = False
  cons x [] = [x]
  cons x y = x:y
  head [] = error "head empty"
  head (x:xs) = x
  tail [] = error "tail empty"
  tail (x:xs) = xs

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- Figure 2.3
instance Stack List where
  empty = Nil
  isEmpty Nil = True
  isEmpty _   = False
  cons a Nil = Cons a Nil
  cons a list = Cons a list
  head Nil = error "head Nil"
  head (Cons a _) = a
  tail Nil = error "tail Nil"
  tail (Cons _ xs) = xs

-- Exercise 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes a@(x:xs) = a : (suffixes  xs)

suffixes' :: [a] -> [[a]]
suffixes' [] = [[]]
suffixes' l = suff l [l]
  where suff [] acc = acc ++ [[]]
        suff a@(x:xs) acc = suff xs (acc ++ [a])

suffixes'' :: [a] -> [[a]]
suffixes'' [] = [[]]
suffixes'' l = suff [l] l
  where suff acc [] = reverse $ [] : acc
        suff acc a@(x:xs) = suff (a:acc) xs

-- Figure 2.7
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

t :: Tree Int
t = (Node 4) (Node 1 Empty $ Node 2 Empty Empty) (Node 7 (Node 5 Empty Empty) Empty)


class Set s where
  emptyS :: s a
  insert :: Ord a => a -> s a -> s a
  member :: Ord a => a -> s a -> Bool

instance Set Tree where
  emptyS = Empty
  insert x Empty = (Node x) Empty Empty
  insert x (Node a left right)
    | x < a     = Node a (insert x left) right
    | x > a     = Node a left (insert x right)
    | otherwise = Node a left right

  member x Empty = False
  member x (Node a left right)
    | x < a     = member x left
    | x > a     = member x right
    | otherwise = True
  {-member x (Node a left right) =
    if (x < a)
      then
        member x left
      else if (x > a)
        then member x right
        else True
  -}

-- Example balanced tree (Node 4) (Node 1 Empty $ Node 2 Empty Empty) (Node 7 (Node 5 Empty Empty) Empty)
-- Should insert 6 into Node 2 empty 3, and 6 into last tree
-- let t = (Node 4) (Node 1 Empty $ Node 2 Empty Empty) (Node 7 (Node 5 Empty Empty) Empty)
-- t == (insert 1 t)
-- insert 3 t == Node 4 (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))) (Node 7 (Node 5 Empty Empty) Empty)
-- insert 6 t == Node 4 (Node 1 Empty (Node 2 Empty Empty)) (Node 7 (Node 5 Empty (Node 6 Empty Empty)) Empty)

-- Exercise 2.2 - See above for member'
member' x Empty = False
member' x t@(Node a left right) = member'' t a
  where
    member'' Empty can = x == can
    member'' (Node a left right)  can
      | x < a     = member'' left can
      | otherwise = member'' right a

-- Exercise 2.3 - insert2

insert2 :: (Ord a) => a -> Tree a -> Either String (Tree a)
insert2 x Empty = return (Node x Empty Empty)
insert2 x (Node a left right)
  | x < a     = liftM (\t -> Node a t right) (insert2 x left)
  | x > a     = liftM (\t -> Node a left t)  (insert2 x right)
  | otherwise = fail "insert2: already exists"

-- TODO - exercise 2.4 - insert3

insert3 :: (Ord a) => a -> Tree a -> Either String (Tree a)
insert3 x Empty = return (Node x Empty Empty)
insert3 x t@(Node a left right) = insert3' t a
  where
    insert3' Empty can
      | x == can    = fail "insert3': already exists"
      | otherwise = return (Node x Empty Empty)
    insert3' (Node a' left' right')  can
      | x < a'    = liftM (\t -> Node a' t right') (insert3' left' can)
      | otherwise = liftM (\t -> Node a' left' t ) (insert3' right' a')

-- exercise 2.5 - a 
complete :: a -> Int -> Tree a
complete x d = complete' 0
  where leaf = (Node x Empty Empty)
        complete' d'
          | d' == d = leaf
          | otherwise = 
              let newTree = (complete' (d' + 1))
                in Node x newTree newTree
-- TODO - exercise 2.5 - b
balanced :: a -> Int -> Tree a
balanced x n
  | n <= 0 = Empty
  | n == 1 = Node x Empty Empty
  | even n  = Empty
  | odd n = Empty
  --where
    --create2 m = 

-- exercise 2.6

type Map k v = Tree (Key k, Val v)
data Key k = Key k
  deriving (Show, Eq)
data Val v = Val v
  deriving (Show)

instance Ord k => Ord (Key k) where
  (Key k) `compare` (Key k') = compare k k'
instance Eq (Val v) where
  _ == _ = True
instance Ord (Val v) where
  compare _ _ = EQ

{-
class FiniteMap m where
  emptyFM :: m a
  bindFM :: Key k -> Val v -> m a -> m a
  lookupFM :: key k -> m a -> Either String (Val v)

--instance FiniteMap m => FiniteMap (Map k v) where
instance FiniteMap Tree where
  emptyFM = Empty
  --bindFM :: (Ord k, Ord v) => Key k -> Val v -> Tree (Key k, Val v) -> Tree (Key k, Val v)
  bindFM k v m = insert (k, v) m
  --lookupFM :: (Ord k, Ord v) => Key k -> Tree (Key k, Val v) -> Either String (Val v)
  lookupFM _ Empty = fail "lookupFM - not there yo"
  lookupFM k (Node (k', v') left right)
    | k < k' = lookupFM k left
    | k > k' = lookupFM k right
    | otherwise = return v'
-}

emptyMap = Empty
insertMap :: (Ord k, Ord v) => Key k -> Val v -> Map k v -> Map k v
insertMap k v m = insert (k, v) m
memberMap :: (Ord k, Ord v) => Key k -> Map k v -> Either String (Val v)
memberMap _ Empty = fail "member - not there yo"
memberMap k (Node (k', v') left right)
  | k < k' = memberMap k left
  | k > k' = memberMap k right
  | otherwise = return v'

kvTree = insertMap (Key 10) (Val 11) $ insertMap (Key 3) (Val 4) $ insertMap (Key 1) (Val 2) Empty

