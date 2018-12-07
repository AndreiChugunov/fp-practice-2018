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

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize = fromInt' . toInt'
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
  quotRem a b = let (q, r) = quotRem' (f a) (f b)
                    m = (signum a)
                in if (signum a == signum b)
                   then (q, m * r)
                   else (negate q, m * r)
    where
      quotRem' a b = case (a `compare` b) of
                      EQ | b == Zero -> error "Division by Zero"
                      EQ | a == Zero -> (Zero, Zero)
                      EQ -> (Succ Zero, Zero)
                      GT -> let (x1, x2) = quotRem' (f (a - b)) b
                            in (x1 + (Succ Zero), Zero + x2)
                      LT -> (Zero, a)
      f = abs . normalize

  toInteger Zero = 0
  toInteger (Succ a) = toInteger a + 1
  toInteger (Pred a) = toInteger a - 1


testL = [11, 7]
zs = [1, -1]
genL = [(z * x, z1 * y) | x <- testL, y <- testL, z <- zs, z1 <- zs, x /= y]

resultion = (\(x, y) -> (quotRem x y) == let (a, b) = (quotRem (fromInt' x) (fromInt' y))
                                         in (toInt' a, toInt' b)
            ) <$> genL




instance Real WeirdPeanoNumber where
  toRational x = toRational $ toInt' x

instance Ord WeirdPeanoNumber where
  a `compare` b = case (normalize a, normalize b) of
                    (Zero, Zero)     -> EQ
                    (Zero, Succ y)   -> LT
                    (Zero, Pred y)   -> GT
                    (Succ x, Succ y) -> x `compare` y
                    (Succ x, _)      -> GT
                    (Pred x, Pred y) -> x `compare` y
                    (Pred x, _)      -> LT


instance Num WeirdPeanoNumber where
  (+) a Zero = a
  (+) Zero b = b
  (+) (Succ a) b = Succ $ a + b
  (+) (Pred a) b = Pred $ a + b

  (*) a b = let f = abs . normalize
            in if (signum a == signum b)
               then multiply (f a) (f b)
               else negate (multiply (f a) (f b))
    where
       multiply Zero y = Zero
       multiply x Zero = Zero
       multiply (Succ x) y = x * y + y

  abs a | a > Zero = a
        | a < Zero = negate a
        | otherwise = Zero

  signum a | a > Zero = 1
           | a < Zero = -1
           | otherwise = 0

  fromInteger a | a > 0 = Succ (fromInteger $ a - 1)
                | a < 0 = Pred (fromInteger $ a + 1)
                | otherwise = Zero

  negate Zero = Zero
  negate (Succ a) = Pred (negate a)
  negate (Pred a) = Succ (negate a)
