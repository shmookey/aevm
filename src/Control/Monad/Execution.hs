{- | Execution monad

  Represents a computation that:

    - May or may not terminate with a resultant value or error
    - Carries its own internal state
    - May access an underlying environment
    - May signal interrupts, which can be handled in an external context
    - May be cancelled by an interrupt handler
    - May be resumed after cancellation

-}

{-# LANGUAGE RankNTypes #-}

module Control.Monad.Execution where

import Prelude hiding (fail)
import Control.Monad (join)

import Control.Monad.Result (Result, mapError)
import Control.Monad.Resultant (ResultantMonad, ResultantT(ResultantT),
  lift, fail, point, runResultantT, getState, setState)
import Control.Monad.Interruptible (InterruptibleT(InterruptibleT)
  , Interruption(Done, Cont), lifti, imap, mapInterrupt, runInterruptibleT)


-- | Execution type definition
type Execution m i s e = ResultantT (InterruptibleT m (i, s)) s e


-- Functions for running Executions
-- ---------------------------------------------------------------------

-- | Run an Execution in the context of another ResultantMonad.
-- 
-- Takes an Execution task, a mapping function to convert the Execution's 
-- errors into the caller's error type, a "runner" that unwraps values of the
-- underlying monad, an interrupt handler that also receives a copy of the
-- Execution's internal state and which can cancel the execution gracefully
-- (or if it fails) and finally an initial state with which to kick the whole
-- process off.
--
-- The result is the final state and (if execution completed) the return value.
execute :: (Monad m, ResultantMonad r e') => Execution m i s e a -> (e -> e') -> (forall b. m (Interruption m (i, s) (s, b)) -> r (Interruption m (i, s) (s, b))) -> (i -> s -> r Bool) -> s -> r (s, Maybe a)
execute task adaptError run handler state = do
  (st, result) <- runHandlerR run handler $ runResultantT task state
  case result of
    Nothing -> return (st, Nothing)
    Just r  -> do x <- point $ join (mapError adaptError <$> r)
                  return (st, Just x)

-- | Cancel-resumable version of `execute`, does not raise errors into caller
executeR :: (Monad m, Monad r) => Execution m i s e a -> (forall b. m (Interruption m (i, s) (s, b)) -> r (Interruption m (i, s) (s, b))) -> (i -> s -> r Bool) -> s -> r (s, Either (Execution m i s e a) (Result e a))
executeR task run handler state = do
  (st, result) <- runResumable run handler $ runResultantT task state
  case result of
    Left task' -> do
      let t = setState st >> (ResultantT $ \st -> task')
      return (st, Left t)
    Right r -> do
      return (st, Right $ join r)

-- | Run an Execution and fail if it is interrupted.
uninterruptible :: (Monad m, ResultantMonad r e') => Execution m i s e a -> (e -> e') -> (i -> e') -> (forall b. m (Interruption m (i, s) (s, b)) -> r (Interruption m (i, s) (s, b))) -> s -> r (s, a)
uninterruptible task adaptError interruptError run state = do
  (st, r) <- execute task adaptError run (alwaysFail interruptError) state
  case r of Just x -> return (st, x) -- the result will never be Nothing because the interrupt handler always fails

-- | Like `uninterruptible`, but discards the final state
query :: (Monad m, ResultantMonad r e') => Execution m i s e a -> (e -> e') -> (i -> e') -> (forall b. m (Interruption m (i, s) (s, b)) -> r (Interruption m (i, s) (s, b))) -> s -> r a
query task adaptError interruptError run state =
  snd <$> uninterruptible task adaptError interruptError run state


-- Functions available to Execution monads
-- ---------------------------------------------------------------------

-- | Interrupt the running Execution from within
interrupt :: Monad m => i -> Execution m i s e ()
interrupt ev = ResultantT 
  (\st -> InterruptibleT . return . Cont (ev, st) $ runResultantT (return ()) st)

-- | Lift a value from the underlying monad
env :: Monad m => m a -> Execution m i s e a
env = lift . lifti


-- Helper functions
-- ---------------------------------------------------------------------

-- | An interrupt handler that ignores the interrupt and cancels the execution
handleNothing :: Monad m => i -> s -> m Bool
handleNothing _ _ = return False

-- | An interrupt handler that always fails, wrapping the interrupt in an error
alwaysFail :: ResultantMonad r e => (i -> e) -> i -> s -> r Bool
alwaysFail f i _ = fail $ f i

-- | Like Interruptible's runHandlerM, but with Result instead of Maybe and the interrupt handler receives state
runHandlerR :: Monad r => (forall b. m (Interruption m (i, s) (s, b)) -> r (Interruption m (i, s) (s, b))) -> (i -> s -> r Bool) -> InterruptibleT m (i, s) (s, a) -> r (s, Maybe (Result e a))
runHandlerR run handler ma = do 
  result <- run $ runInterruptibleT ma
  case result of
    Cont (ev, st) mb -> do
      shouldContinue <- handler ev st
      if shouldContinue
      then runHandlerR run handler mb
      else return (st, Nothing)
    Done (st, x) -> return $ (st, Just $ return x)

-- | Like `runHandlerR`, but also returns the continuation if the execution is cancelled
runResumable :: Monad r => (forall b. m (Interruption m (i, s) (s, b)) -> r (Interruption m (i, s) (s, b))) -> (i -> s -> r Bool) -> InterruptibleT m (i, s) (s, a) -> r (s, Either (InterruptibleT m (i, s) (s, a)) (Result e a))
runResumable run handler ma = do 
  result <- run $ runInterruptibleT ma
  case result of
    Cont (ev, st) mb -> do
      shouldContinue <- handler ev st
      if shouldContinue
      then runResumable run handler mb
      else return (st, Left mb)
    Done (st, x) -> return $ (st, Right $ return x)


-- ---------------------------------------------------------------------

-- Examples which I keep around until I'm sure this all works:

--type Runner = ResultantT IO ([Int], Int) ()
--type Machine = ResultantT (InterruptibleT Env Int) Int ()
--type Env = Resultant Int ()
--
--
--runnerEnv :: Env a -> Runner a
--runnerEnv ma = (fmap snd getState) >>= \q -> case runEnv ma q of
--    Ok x -> return x
--    Err e -> Control.Monad.Resultant.fail e
--
--runEnv :: Env a -> Int -> Result () a
--runEnv ma st = snd $ runResultant ma st
--
--handleMachineOutput :: Int -> Runner Bool
--handleMachineOutput x = do
--  (xs, q) <- getState
--  lift $ putStrLn (show x)
--  setState (x:xs, q)
--  return $ length xs < 20
--
--runMachine :: Machine ()
--runMachine = 
--  let 
--    envState :: Env Int
--    envState = getState
--
--    envStateI :: Machine Int
--    envStateI = lift $ lifti envState
--
--  in forever $ do
--    x <- getState
--    d <- lift (lifti (getState :: Env Int))
--    interrupt x
--    setState (x+d)
--
--startMachine :: Runner ()
--startMachine = do
--  runHandlerM runnerEnv handleMachineOutput (runResultantT runMachine 0)
--  return ()
--
--
--type FizzBuzzer = ResultantT (InterruptibleT Identity String) Int ()
--
--fizzbuzz :: FizzBuzzer ()
--fizzbuzz = forever $ do
--  c <- getState
--  if
--    (c `mod` 3 == 0) && (c `mod` 5 == 0)
--  then
--    interruptR $ show c ++ " fizzbuzz"
--  else if
--    (c `mod` 3 == 0)
--  then
--    interruptR $ show c ++ " fizz"
--  else if
--    (c `mod` 5 == 0)
--  then
--    interruptR $ show c ++ " buzz"
--  else
--    return ()
--  setState (c+1)
--
