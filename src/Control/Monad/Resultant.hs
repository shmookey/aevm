{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Resultant where
--  ( Resultant
--  , ResultantT(ResultantT)
--  , runResultantT
--  , ResultantMonad
--    (point, fail, fromEither, fromMaybe, mapEither, updateError
--    , recover, recoverWith )
--  , lift
--  , runResultant
--  , getState
--  , setState
--  , withState
--  , updateState
--  ) where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity(runIdentity))

import Control.Monad.Result --(Result(Ok, Err), mapError)


-- Resultant
-- ---------------------------------------------------------------------

type Resultant st e = ResultantT Identity st e 

runResultant :: Resultant st e a -> st -> (st, Result e a)
runResultant m st = runIdentity $ runResultantT m st


-- ResultantMonad
-- ---------------------------------------------------------------------

--class Monad m => ResultantMonad m e where
--  point   :: Result e a -> m a
--  reflect :: m a -> m (Result e a)
--  runM    :: m a -> st -> (st, Result e a)
--
--  fail :: e -> m a
--  fail = point . Err
--
--  fromEither :: Either e a -> m a
--  fromEither (Right x)  = return x
--  fromEither (Left err) = fail err
--
--  fromMaybe :: e -> Maybe a -> m a
--  fromMaybe _ (Just x) = return x
--  fromMaybe err _      = fail err
--
--  mapEither :: (e' -> e) -> Either e' a -> m a
--  mapEither _ (Right x)  = return x
--  mapEither f (Left err) = fail $ f err
--
--  updateError :: (e -> e) -> m a -> m a
--  updateError f m = reflect m >>= point . mapError f 
--
--  recover :: (e -> a) -> m a -> m a
--  recover f m = reflect m >>= \r -> case r of
--    Ok x  -> return x
--    Err e -> return $ f e
--
--  recoverWith :: (e -> m a) -> m a -> m a
--  recoverWith f m = reflect m >>= \r -> case r of
--    Ok x  -> return x
--    Err e -> f e
--
--instance ResultantMonad (Result e) e where
--  point   = id 
--  reflect = return
--  fail s = Err s


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

--instance Monad m => ResultantMonad (ResultantT m st e) e where
--  point x = ResultantT $ \st -> return (st, x)
--  reflect m = ResultantT $ \st -> do
--    (st', r) <- runResultantT m st
--    return (st', return r)

-- Utility functions
-- ---------------------------------------------------------------------


-- | Return the current state
--getState :: Monad m => ResultantT m st e st
--getState = ResultantT $ \st -> return (st, return st)
--
---- | Replace the current state with the given value
--setState :: Monad m => st -> ResultantT m st e ()
--setState x = ResultantT $ \_ -> return (x, return ())
--
---- | Run a computation with the given state, discarding the resulting state.
--withState :: Monad m => st -> ResultantT m st e a -> ResultantT m st e a
--withState localState x = 
--  ResultantT $ \st -> do (_, r) <- runResultantT x localState
--                         return (st, r)
--
---- | Transform the current state with the provided function
--updateState :: Monad m => (st -> st) -> ResultantT m st e ()
--updateState f = getState >>= setState . f

-- | Lift a value from the underlying monad into the Resultant
--lift :: Monad m => m a -> ResultantT m st e a
--lift m =
--  let toResult st x = (st, return x)
--  in ResultantT $ \st -> toResult st <$> m

-----------------------


class (Monad (r i s e), Monad i) => Rise (r :: (* -> *) -> * -> * -> * -> *) i s e where
  rrun :: r i s e a -> s -> i (s, Result e a)
  rcon :: (s -> i (s, Result e a)) -> r i s e a

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

  -- Conversions

  fromMaybe :: e -> Maybe a -> r i s e a
  fromMaybe e = \case { Just x -> return x ; Nothing -> fail e }

  fromEither :: Either e a -> r i s e a
  fromEither = \case { Left l -> fail l ; Right r -> return r }



--safeIO :: Rise r IO s e => IO a -> r IO s e a
--safeIO m = 
--  let r = (toResult <$> try m) :: IO (Result SomeException _)
--  in rJoin $ fmap (rPoint . mapError convertException) r
--instance Rise Result e where
--  rrun ma st = return (st, ma)
--  rcon f = snd . runIdentity $ f ()

instance Monad i => Rise ResultantT i s e where
  rrun = runResultantT
  rcon = ResultantT

