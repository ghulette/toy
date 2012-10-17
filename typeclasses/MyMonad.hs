module MyMonad where

import MyFunctor

class MyMonad m where
  pure :: a -> m a
  bind :: m a -> (a -> m b) -> m b

instance MyMonad Box where
  bind (Box x) f = f x
  pure x = Box x

-- Demo using our monad class.

action1' :: Box Int
action1' = pure 5

action2' :: Int -> Box Bool
action2' x = pure (even x)

action' :: Box Bool
action' = 
  action1 `bind` \x ->
  action2 x

-- Same thing, using Haskell's special syntax for the built-in Monad class.

instance Functor Box where
  fmap f (Box x) = Box (f x)

instance Monad Box where
  (Box x) >>= f = f x
  return x = Box x

action1 :: Box Int
action1 = return 5

action2 :: Int -> Box Bool
action2 x = do
  let result = even x
  return result

action :: Box Bool
action = do
  x <- action1
  action2 x
