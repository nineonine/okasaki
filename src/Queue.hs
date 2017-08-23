module Queue where

import Prelude hiding (head, tail, formList)

class Queue q where

  empty   :: q a
  isEmpty :: q a -> Bool

  snoc :: q a -> a -> q a
  head :: q a -> Maybe a
  tail :: q a -> Maybe (q a)

  checkf :: q a -> q a

  fromList :: [a] -> q a

newtype BatchedQueue a = BQ { unQueue :: ([a], [a]) } deriving (Show, Eq)

instance Queue BatchedQueue where

  empty = BQ ([], [])

  isEmpty (BQ (f, r)) = null f

  snoc (BQ (f, r))  x = checkf (BQ (f, x:r))

  tail (BQ ([], _))   = Nothing
  tail (BQ (x:f, r))  = Just $ checkf (BQ (f, r))

  head (BQ ([], _))   = Nothing
  head (BQ (x:f, _j)) = Just x

  checkf (BQ ([], r)) = BQ (reverse r, [])
  checkf q            = q

  fromList [] = BQ ([], [])
  fromList xs = let
    n = div (length xs) 2
    f = take n xs
    r = reverse $ drop n xs
    in BQ (f, r)

