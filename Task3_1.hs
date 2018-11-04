module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber


toInt' :: WeirdPeanoNumber -> Int
toInt' p = toInt'' p 0
  where
    toInt'' Zero k = k
    toInt'' (Succ x) k = toInt'' x (k+1)
    toInt'' (Pred x) k = toInt'' x (k-1)

fromInt' :: Int -> WeirdPeanoNumber
fromInt' p | p > 0 = Succ (fromInt' (p-1))
           | p < 0 = Pred (fromInt' (p+1))
           | otherwise = Zero
  -- Реализуйте все классы типов, которым должны отвечать целые числа
instance Enum WeirdPeanoNumber where
  toEnum x = fromInt' x
  fromEnum x = toInt' x

instance Show WeirdPeanoNumber where
  show Zero = "Zero"
  show (Succ x) = "(Succ " <> show x <> ")"
  show (Pred x) = "(Pred " <> show x <> ")"

instance Eq WeirdPeanoNumber where
  (==) a b = toInt' a == toInt' b

instance Bounded WeirdPeanoNumber where
  minBound = fromInt' minBound
  maxBound = fromInt' maxBound

instance Integral WeirdPeanoNumber where
  quotRem a b = let (a', b') = quotRem (toInt' a) (toInt' b)
                in (fromInt' a', fromInt' b')
  toInteger a = toInteger $ toInt' a

instance Real WeirdPeanoNumber where
  toRational x = toRational $ toInt' x

instance Ord WeirdPeanoNumber where
  a `compare` b | a == b = EQ
                | toInt' a > toInt' b = GT
                | otherwise = LT

instance Num WeirdPeanoNumber where
  (+) a b = fromInt' (toInt' a + toInt' b)
  (*) a b = fromInt' (toInt' a * toInt' b)
  abs a = fromInt' $ abs $ toInt' a
  signum a | toInt' a > 0 = 1
           | toInt' a < 0 = -1
           | otherwise = 0
  fromInteger a = fromInt' $ fromInteger a
  negate a = (Pred Zero) * a
