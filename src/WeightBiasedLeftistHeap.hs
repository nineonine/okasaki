module WeightBiasedLeftistHeap where

-- exercise 3.4b
-- define Weight-Biased Leftist Tree
type Weight = Int
data WBHeap a = E | T Weight a (WBHeap a) (WBHeap a) deriving (Show, Eq)

-- exercise 3.4c
-- implement merge thar
-- runs in one pass
betterMerge :: Ord a => WBHeap a -> WBHeap a -> WBHeap a
betterMerge E h = h
betterMerge h E = h
betterMerge h1@(T w1 x a b) h2@(T w2 y a' b')
  | x <  y    = if weight b + w2 > weight a  then T (w1+w2) x (betterMerge b h2)  a  else T (w1+w2) x a  (betterMerge b h2)
  | otherwise = if w1 + weight a > weight a' then T (w1+w2) y (betterMerge h1 b') a' else T (w1+w2) y a' (betterMerge h1 b')
  where
    weight E            = 0
    weight (T w _ _ _) = w


wbh1 :: WBHeap Int
wbh1 = T 5 4 (T 3 6 (T 1 8 E E) (T 1 6 E E)) (T 1 8 E E)

wbh2 :: WBHeap Int
wbh2 = T 4 3 (T 2 5 (T 1 9 E E) E) (T 1 6 E E)