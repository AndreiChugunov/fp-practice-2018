module Task4_1 where
import Control.Monad (ap, liftM)
-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
  fmap = liftM

instance Applicative FunMonad where
  pure = return
  (<*>) = ap

instance Monad FunMonad where
  return x = FunMonad (\_ -> x)
  x >>= f = FunMonad (\str -> (fun $ f $ (fun x) str) str)
