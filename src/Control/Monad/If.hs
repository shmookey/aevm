module Control.Monad.If where

import Data.Functor.Identity
import Control.Monad (when, unless, void)


ifM :: Monad m => m Bool -> m () -> m ()
ifM c ma = c >>= flip when ma

ifElseM :: Monad m => m Bool -> a -> m a -> m a
ifElseM mc z ma = mc >>= \c -> if c then ma else return z

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c ma = c >>= flip unless ma

