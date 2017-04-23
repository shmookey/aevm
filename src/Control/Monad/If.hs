module Control.Monad.If where

import Control.Monad (when, unless, void)


ifM :: Monad m => m Bool -> m () -> m ()
ifM c ma = c >>= flip when ma

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c ma = c >>= flip unless ma


