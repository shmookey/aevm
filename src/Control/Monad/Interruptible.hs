{-# LANGUAGE RankNTypes #-}

module Control.Monad.Interruptible where

import Data.Functor.Identity (Identity)


data Interruption m e a
  = Done a
  | Cont e (InterruptibleT m e a)

type Interruptible e = InterruptibleT Identity e

newtype InterruptibleT m e a = InterruptibleT { runInterruptibleT :: m (Interruption m e a) }

instance Monad m => Functor (InterruptibleT m e) where
  fmap f fx = InterruptibleT $ do
    result <- runInterruptibleT fx
    return $ case result of
      Done x    -> Done $ f x
      Cont e ct -> Cont e $ fmap f fx

instance Monad m => Applicative (InterruptibleT m e) where
  pure x = InterruptibleT $ return $ Done x
  ff <*> fx = InterruptibleT $ do
    result_f <- runInterruptibleT ff
    case result_f of
      Done f -> do
        result_x <- runInterruptibleT fx
        return $ case result_x of
          Done x      -> Done (f x)
          Cont ev fx' -> Cont ev (fmap f fx')
      Cont ev ff' ->
        return $ Cont ev $ ff' <*> fx

instance Monad m => Monad (InterruptibleT m e) where
  return = pure
  ma >>= f = InterruptibleT $ do
    result <- runInterruptibleT ma 
    case result of
      Done x      -> runInterruptibleT (f x) 
      Cont ev ma' -> return $ Cont ev (ma' >>= f)


break :: Monad m => e -> InterruptibleT m e ()
break ev = InterruptibleT $ return $ Cont ev (return ())

runForever :: Monad m => InterruptibleT m e a -> m [e]
runForever ma = do
  result <- runInterruptibleT ma 
  case result of
    Done _      -> return []
    Cont ev ma' -> runForever ma' >>= \evs -> return (ev:evs)

runHandler :: Monad m => (e -> m Bool) -> InterruptibleT m e a -> m (Maybe a)
runHandler f ma = do
  result <- runInterruptibleT ma
  case result of
    Done x -> return (Just x)
    Cont ev ma' -> do
      shouldCont <- f ev
      if shouldCont
      then runHandler f ma'
      else return Nothing

runHandlerM :: Monad r => (forall b. m b -> r b) -> (e -> r Bool) -> InterruptibleT m e a -> r (Maybe a)
runHandlerM run f ma = do 
  result <- run $ runInterruptibleT ma
  case result of
    Done x    -> return (Just x)
    Cont e mb -> do
      shouldCont <- f e
      if shouldCont
      then runHandlerM run f mb
      else return Nothing

mapInterrupt :: Monad m => (e -> e') -> Interruption m e a -> Interruption m e' a
mapInterrupt f int = case int of
  Done x    -> Done x
  Cont e ma -> Cont (f e) (imap f ma)

imap :: Monad m => (e -> e') -> InterruptibleT m e a -> InterruptibleT m e' a
imap f ma = InterruptibleT $ do
  interruption <- runInterruptibleT ma
  return $ mapInterrupt f interruption

lifti :: Monad m => m a -> InterruptibleT m e a
lifti ma =
  InterruptibleT $ fmap Done ma


