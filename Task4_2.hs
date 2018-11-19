module Task4_2 where
import Control.Monad (ap, liftM)

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
  fmap = liftM

instance Applicative FourOf where
  pure = return
  (<*>) = ap

instance Monad FourOf where
  return x = FourOf x x x x
  (FourOf a b c d) >>= f = let FourOf a' _ _ _ = f a
                               FourOf _ b' _ _ = f b
                               FourOf _ _ c' _ = f c
                               FourOf _ _ _ d' = f d
                           in FourOf a' b' c' d'
