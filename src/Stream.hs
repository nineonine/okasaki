module Stream where

import Prelude hiding (take, drop, reverse, (++), sort)

class Streamy a where

  (++) :: a -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a
  reverse :: a -> a


data Stream a = Nil | Cons a (Stream a) deriving (Show)

instance Streamy (Stream a) where

  Nil         ++ t = t
  (Cons x s)  ++ t = Cons x (s ++ t)

  take 0 s          = s
  take _ Nil        = Nil
  take n (Cons x s) = Cons x (take (n-1) s)

  drop n s = let
    drop' 0 s          = s
    drop' _ Nil        = Nil
    drop' n (Cons x s) = drop' (n-1) s
    in drop' n s

  reverse s = let
    reverse' Nil r        = Nil
    reverse' (Cons x s) r = reverse' s (Cons x r)
    in reverse' s Nil



-- exersize 4.2
-- implement insertion sort
-- on streams
sort :: Ord a => Stream a -> Stream a
sort s = let

  swap x Nil         = Cons x Nil
  swap x (Cons x' t) = if x < x' then Cons x (swap x' t) else Cons x' (swap x t)

  go r  Nil     = r
  go Nil (Cons x t) = go (Cons x Nil) t
  go (Cons  x t) (Cons x' t')
    | x < x'    = go (Cons x(swap x' t)) t'
    | otherwise = go (Cons x'(swap x t)) t'

  in go Nil s

fromList' :: Ord a => [a] -> Stream a
fromList' = foldr Cons Nil