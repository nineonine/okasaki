module PairingHeap where

data Heap a = E | T a [Heap a] deriving Show

findMin :: Heap a -> Maybe a
findMin E       = Nothing
findMin (T x _) = Just x

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E h = h
merge h E = h
merge h1@(T x h1') h2@(T y h2')
  | x <= y    = T x (h2:h1')
  | otherwise = T y (h1:h2')

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (T x [])

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []       = E
mergePairs [h]      = h
mergePairs (x:y:hs) = merge (merge x y) $ mergePairs hs

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin E       = Nothing
deleteMin (T x hs) = Just $ mergePairs hs

-- exersize 5.8(a)
-- write a function 'toBinary'
-- that converts PairingHeap
-- to Binary Tree
data BinTree a = E' | T' a (BinTree a) (BinTree a) deriving (Show)

{-# ANN toBinary "HLint: ignore" #-}
toBinary :: Ord a => Heap a -> BinTree a
toBinary (T x [])     = T' x E' E'
toBinary (T x hs) = T' x (go hs) E' where
  go [] = E'
  go [T y hs] = T' y (go hs) E'
  go ((T y hs):rest) = T' y (go hs) (go rest)


test0 = T 2 [T 5 [], T 6 []]

test1 = T 3 []

test2 = T 4 [T 7[T 8 [], T 9 []]]

tHeap = T 1 [T 2 [T 5 [], T 6 []], T 3 [], T 4 [T 7[T 8 [], T 9 []]]]