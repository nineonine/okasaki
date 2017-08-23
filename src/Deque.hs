module Deque where

import Prelude hiding (head, tail, formList, last, init)

-- exersize 5.1(a)
-- implement Double-Ended Queue
class Deque q where

  empty   :: q a
  isEmpty :: q a -> Bool

  cons :: a -> q a -> q a
  head :: q a -> Maybe a
  tail :: q a -> Maybe (q a)

  snoc :: q a -> a -> q a
  last :: q a -> Maybe a
  init :: q a -> Maybe (q a)

  check :: q a -> q a

  fromList :: [a] -> q a

newtype DQueue a = DQ { unDque :: ([a], [a]) } deriving Show

instance Deque DQueue where

  check dq@(DQ (f, r))
    | null f =
       let
         f' = reverse $ drop (div (length r) 2) r
         r' = take (div (length r) 2) r
       in DQ (f', r')
    | null r =
       let
         f' = take (div (length f) 2) f
         r'  = reverse $ drop (div (length f) 2) f
       in DQ (f', r')
    | otherwise = dq

  empty = DQ ([], [])
  isEmpty (DQ (f, _)) = null f

  cons x (DQ (f, r)) =  check $ DQ (x:f, r)

  head (DQ ([], r)) = Nothing
  head (DQ (x:f, r)) = Just x

  tail (DQ (x:f, r)) = Just $ check $ DQ (f, r)
  tail (DQ ([], r)) = Nothing

  snoc (DQ (f, r)) x = check $ DQ (f, x:r)

  last (DQ (f, x:r)) = Just x
  last (DQ (f, []))  = Nothing

  init (DQ (f, x:r)) = Just $ check $ DQ (f, r)
  init (DQ (f, []))  = Nothing

  fromList [] = DQ ([], [])
  fromList xs = let
    n = div (length xs) 2
    f = take n xs
    r = reverse $ drop n xs
    in DQ (f, r)
