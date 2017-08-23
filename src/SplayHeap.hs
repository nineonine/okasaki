module SplayHeap where

data Tree a = E | T (Tree a) a (Tree a) deriving (Show)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert E

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

insert' :: Ord a => a -> Tree a -> Tree a
insert' x t = let (small, big) = partition x t in T small x big

bigger :: Ord a => a -> Tree a -> Tree a
bigger pivot E        = E
bigger pivot (T a x b)
  | x <= pivot = bigger pivot b
  | otherwise  = case a of
      E -> T E x b
      (T a1 y a2) -> if y <= pivot
        then T (bigger pivot a2) x b
        else T (bigger pivot a1) y (T a2 x b)

-- exersize 5.4
-- implement 'smaller'
-- retain equal elements on your way
-- without redundant equality check
smaller :: Ord a => a -> Tree a -> Tree a
smaller pivot E = E
smaller pivot (T a x b)
  | x > pivot = smaller pivot a
  | otherwise = case b of -- x <= pivot
      E -> T a x E
      (T b1 y b2) -> if y > pivot
        then T a x (smaller pivot b1)
        else T (T a x b1) y (smaller pivot b2)


partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition pivot E = (E, E)
partition pivot t@(T a x b)
  | x <= pivot = case b of
    E -> (t, E)
    (T b1 y b2) -> if y <= pivot
      then let (small, big) = partition pivot b2
           in (T (T a x b1) y small, big)
      else let (small, big) = partition pivot b1
           in  (T a x small, T big y b2)
  | otherwise = case a of
    E -> (E, t)
    (T a1 y a2) -> if y <= pivot
      then let (small, big) = partition pivot a2
           in (T a1 y small, T big x b)
      else let (small, big) = partition pivot a1
           in (small, T big y (T a2 x b))


findMin :: Tree a -> Maybe a
findMin E         = Nothing
findMin (T E x _) = Just x
findMIn (T a x _) = findMin a

deleteMin :: Tree a -> Tree a
deleteMin E         = E
deleteMin (T E _ b) = b
deleteMin (T (T E _ b) y c) = T b y c
deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge E t = t
merge (T a x b) t =
  let (ta, tb) = partition x t
  in T (merge ta a) x (merge tb b)


-- exersize 5.7
-- insert element
-- and dump to list using
-- inorder traversal
insertAndDump :: Ord a => a -> Tree a -> [a]
insertAndDump v E = [v]
insertAndDump v t =
  let (small, big) = partition v t
  in toList small ++ [v] ++ toList big  -- hehe :O


toList :: Tree a -> [a]
toList E = []
toList (T E x b) = x : toList b
toList (T a x E) = toList a ++ [x]
toList (T a x b) = toList a ++ [x] ++ toList b

