module BinarySearchTree where

import Data.Maybe (fromMaybe, isJust)
import Test.QuickCheck hiding (elements)
import System.Random (Random)
import Control.Monad (liftM, liftM2, liftM3)

data Tree a = E | T (Tree a) a (Tree a) deriving (Eq, Show)

-- exercise 2.1
suffixes :: [a] -> [[a]]
suffixes []      = [[]]
suffixes a@(h:t) = a : suffixes t

member :: Ord a => a -> Tree a -> Bool
member x E         = False
member x (T l v r)
  | x < v     = member x l
  | x > v     = member x r
  | otherwise = True

-- exercise 2.2
-- performs d + 1 comparisons
-- where d = depth of a tree
betterMember :: Ord a => a -> Tree a -> Bool
betterMember x t = go t Nothing where
  go (T l v r) track = if x < v then go l track else go r (Just v)
  go E Nothing       = False
  go E (Just track)  = x == track

-- insert
insert :: Ord a => Tree a -> a -> Tree a
insert E x         = T E x E
insert t@(T l v r) x
  | x < v     = T (insert l x) v r
  | x > v     = T l v (insert r x)
  | otherwise = t

-- exercise 2.3
-- insert element into tree
-- without copying entire search path
betterInsert :: Ord a => Tree a -> a -> Tree a
betterInsert t x = fromMaybe t (go t) where
  go E          = Just (T E x E)
  go (T l v r)
    | x < v     = fmap (\t' -> T t' v r) (go l)
    | x > v     = fmap (T l v) (go r)
    | otherwise = Nothing

-- exercise 2.4
-- insert element without copying
-- with d + 1 comparison
evenBetterInsert :: Ord a => Tree a -> a  -> Tree a
evenBetterInsert t x = fromMaybe t (go t Nothing) where
  go (T l v r) track
    | x < v     = fmap (\t' -> T t' v r) (go l track)
    | otherwise = fmap (T l v) (go r $ Just v)
  go E track = case track of
    Just x' -> if x == x' then Nothing else Just (T E x E)
    Nothing -> Just (T E x E)


-- exercise 2.5(a)
-- generate tree in O(d) time
-- where d = tree depth
complete :: a -> Int -> Tree a
complete x 0 = E
complete x d = T t' x t' where t' = complete x (d-1)

-- exercise 2.6(b)
-- create balanced
-- tree of arbitrary size
-- runs in O(log n)
extendedComplete :: a -> Int -> Tree a
extendedComplete x m = fst $ create2 x m where
    create2 x 0 = let e = E in (e, T e x e)
    create2 x m =
        let (_, tPlusOne) = create2 x (m-1)
            mDiv2 = div m 2
            t'    = fst $ create2 x mDiv2
            t''   = fst $ create2 x (mDiv2+1)
        in if odd m
              then (tPlusOne, T t'' x  t')
              else (tPlusOne, T t'  x  t')


-- *-----* --
-- TESTING --
-- *-----* --


-- http://www.seas.upenn.edu/~cis552/12fa/lectures/stub/BST.html
instance (Ord a, Bounded a, Random a, Num a, Arbitrary a) => Arbitrary (Tree a)  where
   arbitrary = gen 0 100 where
      gen :: (Ord a, Num a, Random a) => a -> a -> Gen (Tree a)
      gen min max | (max - min) <= 3 = return E
      gen min max = do
        elt <- choose (min, max)
        frequency [ (1, return E),
                    (6, liftM3 T (gen min (elt - 1))
                            (return elt) (gen (elt + 1) max)) ]


-- check if tree is balanced
isBalancedTree :: Tree a -> Bool
isBalancedTree E = True
isBalancedTree (T l _ r) =
    let diff = abs (size l - size r)
    in  diff <= 1 && isBalancedTree l && isBalancedTree r where
        size E = 0
        size (T l _ r) = size l + size r + 1

-- tree size
size' :: Tree a -> Int
size' E = 0
size' (T l _ r) = size' l + size' r + 1

treeMin :: Tree Int -> Maybe Int
treeMin E = Nothing
treeMin (T l x r) =
   maybes min (treeMin l) (treeMin r)

treeMax :: Tree Int -> Maybe Int
treeMax E = Nothing
treeMax (T l x r) =
   maybes max (treeMax l) (treeMax r)

maybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybes f (Just x) (Just y) = Just (f x y)
maybes _ Nothing m = m
maybes _ m Nothing = m

arbitraryInt :: Gen Int
arbitraryInt = choose (0,100)


-- horrible hacking here !!!!!!!!!!!!
prop_BST :: Tree Int -> Property
prop_BST E = property True
prop_BST (T l x r) = property $ maxL <= x && minR > x where
  maxL = fromMaybe 0 $ treeMax l
  minR = fromMaybe 101 $ treeMin r

prop_member :: Tree Int -> Property
prop_member t = forAll arbitraryInt prop' where
  prop' x = property $ member x $ insert t x

prop_better_member :: Tree Int -> Property
prop_better_member t = forAll arbitraryInt prop' where
  prop' x = property $ betterMember x $ insert t x

prop_insert_preserves :: Tree Int -> Property
prop_insert_preserves t = forAll arbitraryInt prop' where
  prop' x = property $ prop_BST (insert t x)

prop_betterInsert_preserves :: Tree Int -> Property
prop_betterInsert_preserves t = forAll arbitraryInt prop' where
  prop' x = property $ prop_BST (betterInsert t x)

prop_evenBetterInsert_preserves :: Tree Int -> Property
prop_evenBetterInsert_preserves t = forAll arbitraryInt prop' where
  prop' x = property $ prop_BST (evenBetterInsert t x)

prop_complete_is_balanced :: Property
prop_complete_is_balanced = forAll (choose(0,12)) $ \x -> isBalancedTree $ complete () x

prop_extended_complete_is_balanced :: Property
prop_extended_complete_is_balanced = forAll arbitraryInt $ \x -> isBalancedTree $ extendedComplete () x

{-# ANN testChapter2 "HLint: ignore" #-}
testChapter2 :: IO ()
testChapter2 =
  mapM_ quickCheck [
      prop_BST
      , prop_member
      , prop_better_member
      , prop_insert_preserves
      , prop_betterInsert_preserves
      , prop_evenBetterInsert_preserves
      , \_ -> prop_complete_is_balanced
      , \_ -> prop_extended_complete_is_balanced
      ]
