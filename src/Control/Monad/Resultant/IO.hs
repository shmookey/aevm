{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Resultant.IO where

import Prelude hiding (fail, putStr, putStrLn, readFile, writeFile)
import Control.Exception (SomeException)
import System.IO (hFlush, stdout)
import qualified Control.Exception as E
import qualified Prelude as P

import Control.Monad.Result
import Control.Monad.Resultant


class Rise r i s e => RiseIO r i s e where
  io     :: IO a -> r i s e a
  except :: SomeException -> r i s e e

  try :: IO a -> r i s e (Either SomeException a)
  try = io . E.try

  safely :: IO a -> r i s e a
  safely ma = try ma >>= fromEitherUsing except

class Exceptional e where
  fromException :: SomeException -> e

-- | Why have this, when we already have SubError? To save an import of SomeException, that's all.
instance Exceptional e => RiseIO ResultantT IO s e where
  io     = lift 
  except = return . fromException


putStr :: RiseIO r i s e => String -> r i s e ()
putStr x = safely (P.putStrLn x) >> safely (hFlush stdout)

putStrLn :: RiseIO r i s e => String -> r i s e ()
putStrLn = safely . P.putStrLn

readFile :: RiseIO r i s e => FilePath -> r i s e String
readFile = safely . P.readFile

writeFile :: RiseIO r i s e => FilePath -> String -> r i s e ()
writeFile x = safely . P.writeFile x


-- | `attempt "Saving" ma` prints "Saving... " followed by "done." or "failed."
attempt :: RiseIO r i s e => String -> r i s e a -> r i s e a
attempt name ma = do
  putStr $ name ++ "... "
  x <- recoverWith (\e -> putStrLn "failed." >> fail e) ma
  putStrLn "done."
  return x

