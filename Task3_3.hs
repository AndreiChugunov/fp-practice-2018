module Task3_3 where

newtype UnionPSet a = UnionPSet (a -> Bool)
newtype ISecPSet a = ISecPSet (a -> Bool)

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Our Monoids have to follow monoid's laws:
-- (x <> y) <> z = x <> (y <> z) -- associativity
-- mempty <> x = x               -- left identity
-- x <> mempty = x               -- right identity
-- Everything that can follow this rules is accepted.
-- Obviously we have to look at operations with sets.

instance Semigroup (UnionPSet a) where
  (UnionPSet a) <> (UnionPSet b) = UnionPSet (\x -> a x || b x)

instance Monoid (UnionPSet a) where
  mempty = UnionPSet (\_ -> False)
  mappend = (<>) -- This easily follows monoid's laws


instance Semigroup (ISecPSet a) where
  (ISecPSet a) <> (ISecPSet b) = ISecPSet (\x -> a x && b x)

instance Monoid (ISecPSet a) where
  mempty = ISecPSet (\_ -> True) -- This has to be -> True to satisfy l and r
                                 -- identity
  mappend = (<>)

-- Complements
-- Complements has to be symmetric to follow monoid rules
-- There is no way 'Difference' can obey associativity and identity laws.
-- So as 'Cartesian product'.
-- Tho, Symmetric difference can be handled.
newtype SymDiffPSet a = SymDiffPSet (a -> Bool)

instance Semigroup (SymDiffPSet a) where
  (SymDiffPSet a) <> (SymDiffPSet b) = SymDiffPSet (\x -> (a x || b x) && not (a x && b x))

instance Monoid (SymDiffPSet a) where
  mempty = SymDiffPSet (\_ -> False)
  mappend = (<>)

-- Functors is where the magic happening.
-- Here we have only one function fmap with the following signature:
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- The main this is that we have polymorphic function argument not its result:
-- 'a -> Bool'. With the help of funtion (a -> b) we have to change it to
-- 'b -> Bool'. So we have something like this:
-- fmap :: Functor f => ((a -> Bool) -> (b -> Bool)) -> f (a -> Bool) -> f (b -> Bool)
-- There is no way we can apply a function to an argument of a function.

-- Lets asume we have
newtype SomeT a = SomeT (Int -> a)
-- with a function like 'Int -> a' which has polymorphic result.
instance Functor SomeT where
  fmap f (SomeT x) = SomeT (f . x)
-- So we can easily compose them since we can apply function f on a result of
-- function inside SomeT but we can't do it with the function's argument
