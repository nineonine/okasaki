module RedBlackTree where

data Color  = R | B deriving (Show, Eq)
data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show)

instance Eq a => Eq (Tree a) where
  E                == E               = True
  (T c1 l1 x1 r1 ) == (T c2 l2 x2 r2) = c1 == c2 && l1 == l2 && x1 == x2 && r1 == r2
  _                == _               = False

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ l a r)
  | x < a     = member x l
  | x > a     = member x r
  | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x s =
  let ins E         = T R E x E
      ins t@(T c l a r)
        | x < a       = balance c (ins l) a r
        | x > a       = balance c l a (ins r)
        | otherwise   = t
      T _ l' y r' = ins s -- guaranteed to be non-empty
  in T B l' y r'


balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c t1 x t2                   = T c t1 x t2

-- exersize 3.9
-- write fromOrdList
-- which runs in O(n)
fromOrdList :: Ord a => [a] -> Tree a
fromOrdList []     = E
fromOrdList (x:xs) = foldl (flip insert) (T R E x E) xs

-- exersize 3.10(a)
-- improve split balance function
-- into 2: lbalance and balance
-- to avoid unnecessary tests
lbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c t1 x t2                   = T c t1 x t2

rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c t1 x t2                   = T c t1 x t2

insert' :: Ord a => a -> Tree a -> Tree a
insert' x s =
  let ins E         = T R E x E
      ins t@(T c l a r)
        | x < a       = lbalance c (ins l) a r
        | x > a       = rbalance c l a (ins r)
        | otherwise   = t
      T _ l' y r' = ins s -- guaranteed to be non-empty
  in T B l' y r'

-- for testing purposes
fromOrdList' :: Ord a => [a] -> Tree a
fromOrdList' []     = E
fromOrdList' (x:xs) = foldl (flip insert') (T R E x E) xs

-- exersize 3.10(b)
-- avoid checking tree grandchildren
-- which are not on the search path
llbalance :: Color -> Tree a -> a -> Tree a -> Tree a
llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
llbalance c t1 x t2                   = T c t1 x t2

lrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lrbalance c t1 x t2                   = T c t1 x t2

rlbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rlbalance c t1 x t2                   = T c t1 x t2

rrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rrbalance c t1 x t2                   = T c t1 x t2

isRedLeftGChild :: Tree a -> Bool
isRedLeftGChild (T _ (T R _ _ _) _ _) = True
isRedLeftGChild _                     = False

isRedRightGChild :: Tree a -> Bool
isRedRightGChild (T _ _ _ (T R _ _ _)) = True
isRedRightGChild _                     = False

insert'' :: Ord a => a -> Tree a -> Tree a
insert'' x s =
  let ins E         = T R E x E
      ins t@(T c l a r)
        | x < a       = if isRedLeftGChild  t then llbalance c (ins l) a r else lrbalance c (ins l) a r
        | x > a       = if isRedRightGChild t then rrbalance c l a (ins r) else rlbalance c l a (ins r)
        | otherwise   = t
      T _ l' y r' = ins s -- guaranteed to be non-empty
  in T B l' y r'

-- testing
fromOrdList'' :: Ord a => [a] -> Tree a
fromOrdList'' []     = E
fromOrdList'' (x:xs) = foldl (flip insert'') (T R E x E) xs