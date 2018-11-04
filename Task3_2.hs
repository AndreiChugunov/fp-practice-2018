module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

instance Foldable ReverseList where
  foldr f z RNil = z
  foldr f z (RCons xs x) = f x (foldr f z xs)

rlistToList :: ReverseList a -> [a]
rlistToList = foldl (\s x -> x:s) []

listToRList :: [a] -> ReverseList a
listToRList = foldl (\s x -> RCons s x) RNil

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
instance (Show a) => Show (ReverseList a) where
  show RNil = "RNil"
  show (RCons xs x) = "(RCons " <> show xs <> " " <> show x <> ")"

instance (Eq a) => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) _    RNil = False
  (==) RNil _    = False
  (==) (RCons xs x) (RCons ys y) | x == y = xs == ys
                                 | otherwise = False


instance (Ord a) => Ord (ReverseList a) where
  (RCons xs x) `compare` (RCons ys y) | x > y = GT
                                      | x < y = LT
                                      | otherwise = xs `compare` ys
  RNil `compare` RNil = EQ
  RNil `compare` (RCons ys y) = LT
  (RCons xs x) `compare` RNil = GT

instance Semigroup (ReverseList a) where
  (<>) xs ys = foldr (\x s -> RCons s x) ys xs

instance Monoid (ReverseList a) where
  mempty = RNil
  mappend = (<>)

instance Functor ReverseList where
  fmap f RNil = RNil
  fmap f (RCons ys y) = RCons (f <$> ys) (f y)
