module BinomialHeap where


type Rank = Int
data Tree a = Node Rank a [Tree a] deriving (Show, Eq)
type Heap a = [Tree a]


link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r+1) x1 (t2:c1)
  | otherwise = Node (r+1) x2 (t1:c2)

singleton :: Ord a => a -> Tree a
singleton x = Node 0 x []

rank :: Tree a -> Rank
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = return t
insTree t ts@(:ts')
  | rank t < rank t' = t : ts
  | otherwise        = insTree (link t t') ts'

{-# ANN hInsert "HLint: ignore" #-}
hInsert :: Ord a => a -> Heap a -> Heap a
hInsert x t = insTree (Node 0 x []) t

hMerge :: Ord a => Heap a -> Heap a -> Heap a
hMerge [] h = h
hMerge h [] = h
hMerge h1@(t1:ts1) h2@(t2:ts2)
  | rank t1 < rank t2 = t1 : hMerge ts1 h2
  | rank t1 > rank t2 = t2 : hMerge ts2 h1
  | otherwise         = insTree (link t1 t2) (hMerge ts1 ts2)

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [t]    = (t, [])
removeMinTree (t:ts) =
   let (t', ts') = removeMinTree ts
   in if root t <= root t'
         then (t, ts)
         else (t', t:ts)

hDeleteMin :: Ord a => Heap a -> Heap a
hDeleteMin h = let (Node _ _ ts, h') = removeMinTree h in hMerge (reverse ts) h'

hFindMin :: Ord a => Heap a ->  a
hFindMin ts = let (t, _) = removeMinTree ts in root t



genHeap :: [Int] -> Heap Int
genHeap = foldl step [singleton 10] where
    step :: Ord a => Heap a -> a -> Heap a
    step a x = flip insTree a $ singleton x


-- exersize 3.5
-- defive findMin without
-- calling removeMinTree
myFindMin :: Ord a => Heap a -> Maybe a
myFindMin []     = Nothing
myFindMin (t:ts) = go (root t) ts where
  go x (t':ts')  = if x <= root t' then Just x else go (root t') ts'
  go x []        = Just x


-- exersize 3.6
-- factor out Rank
-- from Tree implementation
data Tree' a = Node' a [Tree' a] deriving (Show, Eq)
type Heap' a = [(Rank, Tree' a)]

-- implementation of all necessary
-- functions is trivial having
-- " rank' " function
rank' :: Tree' a -> Rank
rank' (Node' _ []) = 0
rank' (Node' _ ts) = length ts


-- exersize 3.7
-- implement  heaps
-- with findMin operation in O(1)
class Heapish h where
  findMin   :: Ord a => h a -> Maybe a
  insert    :: Ord a => a -> h a -> h a
  merge     :: Ord a => h a -> h a -> h a
  deleteMin :: Ord a => h a -> h a
  empty     :: h a

  hFromList :: Ord a => [a] -> h a
  hFromList = foldr insert empty

data ExtendedHeap h a = E | NE a (h a) deriving (Show)

instance Heapish h => Heapish (ExtendedHeap h) where

  empty = E

  findMin E        = Nothing
  findMin (NE a h) = Just a

  insert a E        = NE a (insert a empty)
  insert a (NE x h) = NE (min a x) (insert a h)

  merge E h = h
  merge h E = h
  merge (NE a h) (NE b h') = NE (min a b) (merge h h')

  deleteMin E        = E
  deleteMin (NE a h) = let h' = deleteMin h
                       in case findMin h' of
                            Nothing -> NE a h'
                            Just x  -> NE x h'

newtype ExampleHeap a = ExampleHeap { unHeap :: Heap a} deriving (Show)

instance Heapish ExampleHeap where
  empty                                  = ExampleHeap []
  findMin (ExampleHeap h)                = myFindMin h
  insert a (ExampleHeap h)               = ExampleHeap $ hInsert a h
  merge (ExampleHeap h) (ExampleHeap h') = ExampleHeap $ hMerge h h'
  deleteMin (ExampleHeap h)              = ExampleHeap $ hDeleteMin h

type BinomialHeap = ExtendedHeap ExampleHeap

fromList'' :: Ord a => [a] -> BinomialHeap a
fromList'' = hFromList


-- exersize 6.5
-- implement SizedHeap
-- to explicitly manage
-- the size of a heap
data SizedHeap h a = E' Int | SH' Int (h a) deriving (Show)

instance Heapish h => Heapish (SizedHeap h) where

  empty = E' 0

  findMin (E' _)    = Nothing
  findMin (SH' _ h) = findMin h

  insert a (E' _)    = SH' 1 (insert a empty)
  insert a (SH' s h) = SH' (s+1) (insert a h)

  merge (E' _) h = h
  merge h (E' _) = h
  merge (SH' s1 h1) (SH' s2 h2) = SH' (s1+s2) (merge h1 h2)

  deleteMin (E' n) = E' n
  deleteMin (SH' s h) = SH' (s-1) (deleteMin h)

type SizedheapExample = SizedHeap ExampleHeap

size :: Ord a => SizedheapExample a -> Int
size (E' _)    = 0
size (SH' s _) = s
