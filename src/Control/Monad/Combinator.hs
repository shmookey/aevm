module Control.Monad.Combinator where

import Control.Monad (void)


tie :: Monad m => m a -> m b -> m (a, b)
tie ma mb = do
  x <- ma
  y <- mb
  return (x, y)

both :: Monad m => (a -> m b) -> (a -> m c) -> a -> m (b, c)
both f g x = do
  a <- f x
  b <- g x
  return (a, b)

both_ :: Monad m => (a -> m b) -> (a -> m c) -> a -> m ()
both_ f g x = void $ both f g x

to :: Monad m => (m a, m b) -> (a -> b -> m c) -> m c
to = (>>==)

(>>==) :: Monad m => (m a, m b) -> (a -> b -> m c) -> m c
(ma, mb) >>== f = do
  x <- ma
  y <- mb
  f x y

infixl 8 >>==

--infixr 8 `to`
