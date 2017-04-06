module Test.Control.Monad.ExecutionSpec where

import Prelude hiding (log)

import Control.Monad (forever)

import Control.Monad.Result as R
import Control.Monad.Resultant as Rt
import Control.Monad.Interruptible as I
import Control.Monad.Execution as E 


-- Interruptible fizzbuzz machine:
--   * Interrupt with fizz/buzz/fizzbuzz on multiples of 3 or 5
--   * Add current state number to environment on multiples of 5
--   * Be canceled and resumed on multiples of both 3 and 5
--   * Error on 50
type Machine   = Execution Env Interrupt MState MError
data Interrupt = Fizz | Buzz | FizzBuzz deriving (Show)
data MError    = LimitReached deriving (Show)
type MState    = Int

-- Environment that keeps a list of "buzz" messages from the machine
type Env    = Resultant EState ()
type EState = [Int]

-- Supervisor to manage the execution
type Supervisor = ResultantT IO (MState, EState) String


operate :: Machine ()
operate = forever $ do
  i <- getState
  if i `mod` 3 == 0 && i `mod` 5 == 0
  then interrupt FizzBuzz
  else if i `mod` 3 == 0
  then interrupt Fizz
  else if i `mod` 5 == 0
  then do env . updateState $ (:) i
          interrupt Buzz
  else if i == 50
  then Rt.fail LimitReached
  else return ()
  setState $ i + 1

interruptHandler :: Interrupt -> MState -> Supervisor Bool
interruptHandler x mst1 = do
  (mst2, est) <- getState
  logInt $ "got machine interrupt "         ++ show x
  logInt $ "the received machine state is " ++ show mst1
  logInt $ "the stored machine state is "   ++ show mst2
  logInt $ "the stored env state is "       ++ show est
  logInt $ "updating stored machine state"
  setState (mst1, est)
  case x of
    Fizz     -> return True
    Buzz     -> return True
    FizzBuzz -> return False

runEnv :: Env a -> Supervisor a
runEnv ma = do
  logEnv $ "running an env operation"
  (mst, est) <- getState
  logEnv $ "the current stored machine state is "   ++ show mst
  logEnv $ "the current stored env state is "       ++ show est
  let (est', r) = runResultant ma est
  logEnv $ "the returned env state is "             ++ show est'
  logEnv $ "updating stored env state"
  setState (mst, est')
  case r of Ok x -> do logEnv "the env operation returned a successful result"
                       return x

start :: Supervisor ()
start = do
  log $ "starting machine"
  (mst1, est1) <- getState
  log $ "the initial stored machine state is "   ++ show mst1
  log $ "the initial stored env state is "       ++ show est1
  (mst, lr)    <- executeR operate runEnv interruptHandler mst1
  (mst2, est2) <- getState
  log $ "the returned machine state is "         ++ show mst
  log $ "the current stored machine state is "   ++ show mst2
  log $ "the current stored env state is "       ++ show est2
  case lr of
    Left cont     -> do logCnt "executeR returned continuation"
                        logCnt "enter with returned machine state and stored env state"
                        (mst', lr') <- executeR cont runEnv interruptHandler mst
                        (mst3, est3) <- getState
                        logCnt $ "the returned machine state is "         ++ show mst'
                        logCnt $ "the current stored machine state is "   ++ show mst3
                        logCnt $ "the current stored env state is "       ++ show est3
    Right (Ok _)  -> log "executeR returned a successful result"
    Right (Err e) -> log $ "executeR returned error " ++ show e
    
logInt :: String -> Supervisor ()
logInt x = lift . putStrLn $ "        INT " ++ x

logEnv :: String -> Supervisor ()
logEnv x = lift . putStrLn $ "    ENV " ++ x

logCnt :: String -> Supervisor ()
logCnt x = lift . putStrLn $ "            CNT " ++ x

log :: String -> Supervisor ()
log = lift . putStrLn


main :: IO ()
main = do
  putStrLn "running supervisor with machine state 1 and env state []"
  ((mst,est), r) <- runResultantT start (1, [])
  putStrLn $ "the final machine state is " ++ show mst
  putStrLn $ "the final env state is "     ++ show est
  case r of
    Ok _  -> putStrLn "supervisor successful result"
    Err e -> putStrLn $ "supervisor returned error " ++ show e


  
