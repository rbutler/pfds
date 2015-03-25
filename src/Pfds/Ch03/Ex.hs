module Pfds.Ch03.Ex where

class Heap h where
  empty   :: Ord a => h a
  isEmpty :: Ord a => h a -> Bool
  insert  :: Ord a => a -> h a -> h a
  merge   :: Ord a => h a -> h a -> h a
  findMin :: Ord a => h a -> a
  deleteMin :: Ord a => h a -> h a

data LeftistHeap a = Empty | Node Int (LeftistHeap a) a (LeftistHeap a)
  deriving (Show)

rank Empty = 0
rank (Node r _ _ _) = r
makeT a x b
  | rank a >= rank b = Node (rank b + 1) a x b
  | otherwise        = Node (rank a + 1) b x a

instance Heap LeftistHeap where
  empty = Empty
  isEmpty Empty = True
  isEmpty _ = False
  insert x h = merge (Node 1 Empty x Empty) h
  
  merge h Empty = h
  merge Empty h = h
  merge h1@(Node _ a1 x b1) h2@(Node _ a2 y b2)
    | x <= y    = makeT a1 x (merge b1 h2)
    | otherwise = makeT a2 y (merge h1 b2)

  findMin Empty = error "Empty Leftist Heap"
  findMin (Node _ _ x _) = x

  deleteMin Empty = error "Empty Leftist Heap"
  deleteMin (Node _ a x b) = merge a b

-- Ex 3.2

-- insert' x h@(Node r a root b) 
  -- | x 

-- Ex 3.3

fromList :: Ord a => [a] -> LeftistHeap a
fromList l = head $ keepMergingPairs $ singletons
  where singletons = map (\e -> Node 1 Empty e Empty) l
        mergePairs (x:y:xs) = (merge x y) : mergePairs xs
        mergePairs [x]      = [x]
        mergePairs []       = []
        keepMergingPairs [x] = [x]
        keepMergingPairs x = keepMergingPairs (mergePairs x)


-- merge (insert 3 Empty) $ insert 1 Empty
--fromList [1,7,3,4,5,9,2]

-- Ex 3.4
data WeightBiasedLeftistHeap a = WBEmpty | WBNode Int (WeightBiasedLeftistHeap a) a (WeightBiasedLeftistHeap a)
  deriving (Show)

weight WBEmpty = 0
weight (WBNode r _ _ _) = r
wbMakeT a x b
  | weight a >= weight b = WBNode (weight b + 1) a x b
  | otherwise        = WBNode (weight a + 1) b x a

instance Heap WeightBiasedLeftistHeap where
  empty = WBEmpty
  isEmpty WBEmpty = True
  isEmpty _ = False
  insert x h = merge (WBNode 1 WBEmpty x WBEmpty) h

  merge h WBEmpty = h
  merge WBEmpty h = h
  merge h1@(WBNode _ a1 x b1) h2@(WBNode _ a2 y b2)
    | x <= y    = wbMakeT a1 x (merge b1 h2)
    | otherwise = wbMakeT a2 y (merge h1 b2)

  findMin WBEmpty = error "Empty Weight Biased Leftist Heap"
  findMin (WBNode _ _ x _) = x

  deleteMin WBEmpty = error "Empty Weight Biased Leftist Heap"
  deleteMin (WBNode _ a x b) = merge a b
