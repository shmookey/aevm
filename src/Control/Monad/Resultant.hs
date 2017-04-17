{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Resultant where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity(runIdentity))

import Control.Monad.Result --(Result(Ok, Err), mapError)


-- Resultant
-- ---------------------------------------------------------------------

type Resultant st e = ResultantT Identity st e 

runResultant :: Resultant st e a -> st -> (st, Result e a)
runResultant m st = runIdentity $ runResultantT m st


-- ResultantT
-- ---------------------------------------------------------------------

newtype ResultantT m st e a = ResultantT { runResultantT :: st -> m (st, Result e a) }

instance Monad m => Functor (ResultantT m st e) where
  fmap f x = ResultantT $ \st -> do (st', r) <- runResultantT x st
                                    return (st', fmap f r)

instance Monad m => Applicative (ResultantT m st e) where
  pure x    = ResultantT $ \st -> return (st, pure x)
  ff <*> fx = ResultantT $ \st -> do (st' , rf) <- runResultantT ff st
                                     (st'', rx) <- runResultantT fx st'
                                     return (st'', rf <*> rx)

instance Monad m => Monad (ResultantT m st e) where
  return   = pure
  ma >>= f = ResultantT $ \st -> do (st', ra) <- runResultantT ma st
                                    rb        <- return (f <$> ra)
                                    case rb of Ok mb -> runResultantT mb st'
                                               Err e -> return (st', Err e)

instance Monad i => Rise ResultantT i s e where
  rrun = runResultantT
  rcon = ResultantT


-- Utility functions
-- ---------------------------------------------------------------------

class (Monad (r i s e), Monad i) => Rise (r :: (* -> *) -> * -> * -> * -> *) i s e where
  rrun :: r i s e a -> s -> i (s, Result e a)
  rcon :: (s -> i (s, Result e a)) -> r i s e a

  rdo  :: r i s e a -> s -> i (Result e a)
  rdo ma s = snd <$> rrun ma s

  point :: Result e a -> r i s e a
  point x = rcon $ \s -> return (s, x)

  lift :: i a -> r i s e a
  lift m = rcon $ \s -> fmap ((,) s . return) m

  rjoin :: i (r i s e a) -> r i s e a
  rjoin m = rcon $ \s -> (m >>= flip rrun s)

  fail :: e -> r i s e a
  fail = point . Err

  -- State access

  getState :: r i s e s
  getState = rcon $ \s -> rrun (return s :: r i s e s) s

  setState :: s -> r i s e ()
  setState x = rcon $ \_ -> rrun (return () :: r i s e ()) x

  updateState :: (s -> s) -> r i s e ()
  updateState f = getState >>= setState . f

  -- Error handling

  revive :: r i s e a -> r i s e (Result e a)
  revive m = rcon $ \s -> do (s', r) <- rrun m s
                             return (s', return r)

  errMap :: Rise r i s e' => (e -> e') -> r i s e a -> r i s e' a
  errMap f m = rcon $ \s -> do (s', r) <- rrun m s
                               return (s', mapError f r)

  recoverWith :: (e -> r i s e a) -> r i s e a -> r i s e a
  recoverWith f ma = revive ma >>= \case { Ok x -> return x ; Err e -> f e }

  recover :: (e -> a) -> r i s e a -> r i s e a
  recover f ma = \case { Ok x -> x ; Err e -> f e } <$> revive ma

  withDefault :: a -> r i s e a -> r i s e a
  withDefault a ma = \case { Ok x -> x ; Err _ -> a } <$> revive ma

  untilError :: r i s e a -> r i s e a
  untilError ma = do
    _ <- ma
    untilError ma

  -- Conversions

  fromMaybe :: e -> Maybe a -> r i s e a
  fromMaybe e = \case { Just x -> return x ; Nothing -> fail e }

  fromEither :: Either e a -> r i s e a
  fromEither = \case { Left l -> fail l ; Right r -> return r }

  fromEitherWith :: (e' -> e) -> Either e' a -> r i s e a
  fromEitherWith f = \case { Left l -> fail $ f l ; Right r -> return r }

  fromEitherUsing :: (e' -> r i s e e) -> Either e' a -> r i s e a
  fromEitherUsing f = \case { Left l -> f l >>= fail ; Right r -> return r }


class SubError e e' where
  suberror :: e' -> e


runIn :: (Rise r1 i1 s1 e1, Rise r2 i2 s2 e2, SubError e1 e2) => r2 i2 s2 e2 a -> s2 -> i2 (r1 i1 s1 e1 a)
runIn ma st = do
  result <- rdo ma st
  return . point $ mapError suberror result

runWith :: (Rise r1 i s1 e1, Rise r2 i s2 e2, SubError e1 e2) => r2 i s2 e2 a -> s2 -> r1 i s1 e1 a
runWith ma st =
  rjoin (point . mapError suberror <$> rdo ma st)

subpoint :: (Rise r i s e, SubError e e') => Result e' a -> r i s e a
subpoint r = point $ mapError suberror r


