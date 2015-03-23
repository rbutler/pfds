{-# LANGUAGE TypeSynonymInstances #-}
module Pfds.Ch02.Ex where
import Control.Monad (liftM)
import Control.Monad.Trans.Class



class Stack s where
  empty   :: s a
  isEmpty :: s a -> Bool
  cons    :: a -> s a -> s a
  head    :: s a -> a
  tail    :: s a -> s a

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Stack [] where
  empty = []
  isEmpty [] = True
  isEmpty _ = False
  cons x list = x : list
  head [] = error "Nil for head"
  head (x:xs) = x
  tail [] = error "Nil for tail"
  tail (x:xs) = xs


instance Stack List where
  empty = Nil
  isEmpty Nil = True
  isEmpty _ = False
  cons a list = Cons a list
  head Nil = error "Nil for head"
  head (Cons a _) = a
  tail Nil = error "Nil for tail"
  tail (Cons _ s) = s

--suffixes' [1,2,3,4] = [[1,2,3,4], [2,3,4], [3,4], [4], []]

suffixes :: List a -> List (List a)
suffixes Nil = Cons Nil Nil
suffixes list@(Cons x xs) = cons list (suffixes xs)

suffixes' :: [a] -> [[a]]
suffixes' [] = [[]]
suffixes' list@(x:xs) = list : suffixes' xs

class Set s where
  emptyS :: s a
  insert :: (Ord a) => a -> s a ->  s a
  member :: (Ord a) => a -> s a -> Bool
  member' :: (Ord a) => a -> s a -> Bool
  insert' :: (Ord a) => a -> s a -> Either String (s a)
  insert'' :: (Ord a) => a -> s a -> Either String (s a)

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Set Tree where
  emptyS = Empty
  insert x Empty = Node Empty x Empty
  insert x tree@(Node left root right)
    | x < root  = Node (insert x left) root right
    | x > root  = Node left root (insert x right)
    | otherwise = tree
  member x Empty = False
  member x tree@(Node left root right)
    | x < root = member x left
    | x > root = member x right
    | otherwise = x == root
  member' x Empty = False
  member' x tree@(Node _ start _) = remember start tree
    where remember candidate Empty = candidate == x
          remember candidate t2@(Node left root right)
            | x < root  = remember candidate left
            | otherwise = remember root right
  -- insert' :: (Ord a) => a -> s a -> Either String (s a)
  insert' x Empty = return (Node Empty x Empty)
  insert' x tree@(Node left root right)
    | x < root  = liftM (\t -> Node t root right) (insert' x left) 
    | x > root  = liftM (\t -> Node left root t) (insert' x right)
    | otherwise = fail "insert': This element exists"
  insert'' x Empty = return (Node Empty x Empty)
  insert'' x tree@(Node _ start _) = reinsert start tree
    where reinsert candidate Empty
            | x == candidate = fail "insert'': This element exists yo"
            | otherwise      = return (Node Empty x Empty)
          reinsert candidate t2@(Node left root right)
            | x < root  = liftM (\t -> Node t root right) (reinsert candidate left)
            | otherwise = liftM (\t -> Node left root t) (reinsert root right)
--complete :: 

memberDudes :: (Ord a) => a -> Either String (Tree a) -> Bool
memberDudes x (Left y) = False
memberDudes x (Right y) = member x y 


y = Node (Node Empty 1 Empty) 3 (Node Empty 5 Empty)

complete :: a -> Int -> Tree a
complete x d = complete' 0
  where leaf = (Node Empty x Empty)
        complete' d'
          | d' == d = leaf
          | otherwise = 
              Node newTree x newTree
                where newTree = (complete' (d' + 1))
              -- let newTree = (complete' (d' + 1))
                -- in Node newTree x newTree

balanced :: a -> Int -> Tree a
balanced x n
  | n <= 0 = Empty
  | n == 1 = Node Empty x Empty
  | even n  = Empty
  | odd n = Empty


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
   _ `compare` _ = EQ

emptyMap = Empty
insertMap :: (Ord k, Ord v) => Key k -> Val v -> Map k v -> Map k v
insertMap k v m = insert (k, v) m
memberMap :: (Ord k, Ord v) => Key k -> Map k v -> Either String (Val v)
memberMap _ Empty = fail "member - not there yo"
memberMap k (Node left (k', v') right)
  | k < k' = memberMap k left
  | k > k' = memberMap k right
  | otherwise = return v'

kvTree = insertMap (Key (-3)) (Val 0) $ insertMap (Key 5) (Val 12398127391823) $ insertMap (Key 10) (Val 11) $ insertMap (Key 3) (Val 4) $ insertMap (Key 1) (Val 2) Empty












  -- member' x Empty = False
  -- member' x t@(Node _ r _) = f t r
    -- where
      -- f Empty candidate  = x == candidate
      -- f tree@(Node left root right) candidate
        -- | x < root = f left candidate
        -- | otherwise = f right root
insert2 :: (Ord a) => a -> Tree a -> Either String (Tree a)
insert2 x Empty = return (Node Empty x Empty)
insert2 x (Node left a right)
  | x < a     = liftM (\t -> Node t a right) (insert2 x left)
  | x > a     = liftM (\t -> Node left a t)  (insert2 x right)
  | otherwise = fail "insert2: already exists"

