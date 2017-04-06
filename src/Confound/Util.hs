module Confound.Util where

import Prelude hiding (fail)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Result
import Control.Monad.Resultant

import Confound.Types


mapGet :: (Show k, Ord k) => k -> Map k v -> App v
mapGet k kvs = case Map.lookup k kvs of
  Just v -> return v
  Nothing -> fail $ "no such key: " ++ (show k)

logInfo :: String -> App ()
logInfo x = getVerbosity >>= \i ->
  if i >= 2
  then lift $ putStrLn x
  else return ()

logError :: String -> App ()
logError x = getVerbosity >>= \i ->
  if i >= 0
  then lift $ putStrLn x
  else return ()


