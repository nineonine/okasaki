module LeftistHeap where

type Rank = Int
data Heap a = E | T Rank a (Heap a) (Heap a) deriving (Show)
instance Eq a => Eq (Heap a) where
    E == E                       = True
    (T r1 x a b) == (T r2 y a' b') = x == y && r1 == r2 && a == a' && b == b'
    _ == _                       = False

merge :: Ord a => Heap a -> Heap a ->  Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T r y a2 b2)
    | x <= y    = makeT x a1 (merge b1 h2)
    | otherwise = makeT y a2 (merge h1 b2)

rank :: Heap a -> Int
rank E           = 0
rank (T r _ _ _) = r

makeT :: Ord a => a -> Heap a -> Heap a -> Heap a
makeT x h1 h2
    | rank h1 >= rank h2 = T (rank h2 + 1) x h1 h2
    | otherwise          = T (rank h1 + 1) x h2 h1

{-# ANN insert "HLint: ignore" #-}
insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (T 1 x E E) h

findMin :: Heap a -> Maybe a
findMin E           = Nothing
findMin (T _ x _ _) = Just x

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin E           = Nothing
deleteMin (T _ x a b) = Just $ merge a b

singleton :: a -> Heap a
singleton x = T 1 x E E

-- exercise 3.2
-- implement insert
-- without using merge
insert' :: Ord a => a -> Heap a -> Heap a
insert' x E           = T 1 x E E
insert' x (T r y E b) = T r y (singleton x) b
insert' x h@(T r y a b)
  | x <= y     = T 1 x h E
  | otherwise = updateRanks . manageSwaps $ T r y a updatedB where
      updatedB = insert' x b

      updateRanks E = E
      updateRanks (T _ x a b) = T (rank b + 1) x a b

      manageSwaps E = E
      manageSwaps h'@(T r v a' b')
        | a' == E            = T r v b' E
        | rank a' >= rank b' = h'
        | otherwise          = T (rank b' + 1) v b' a'

-- exercise 3.3
-- convert list to heap
-- should run in O(n) time
-- with O(log n) passes
fromList :: Ord a => [a] -> Heap a
fromList []  = E
fromList as = inHalves $ fmap singleton as where
  inHalves [x]   = x
  inHalves [x,y] = merge x y
  inHalves l@(x:xs)
    | even $ length l = let n' = div (length l) 2
                            h' =  (inHalves $ take n' l)
                            h''=  (inHalves $ drop n' l)
                        in merge h' h''
    | otherwise       = let h' = inHalves xs
                        in merge x h'

