{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Parallel where

import Prelude hiding (fail)
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Functor.Identity (Identity(runIdentity))
import Data.ByteString (ByteString)

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Execution (executeLog)

import Fluidity.Common.Binary
import Fluidity.EVM.Core.System (Sys, Error, State, Break)
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Core.Interrupt (Interrupt(..))
import Fluidity.EVM.Data.Transaction (MessageCall(..))
import Fluidity.EVM.Analyse.Outcome (CallReport(CallReport), PostMortem)
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Analyse.Outcome as Outcome
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.Interrupt as INT


postmortems :: [ByteString] -> (ByteString -> MessageCall) -> Blockchain.State -> [PostMortem]
postmortems addrs mkCall chain =
  let
    call :: ByteString -> PostMortem
    call addr = Outcome.postMortem (CallReport msg xs r)
      where (xs, r) = run $ Sys.call msg
            msg     = mkCall addr

    run :: Sys a -> ([Interrupt], Break a)
    run ma = Sys.runLog ma $ Sys.initState { Sys.stChain = chain }

  in
    map call addrs `using` parList rdeepseq


-- Base parallelisation functionality
-- ---------------------------------------------------------------------

--runTask :: Task (Sys.State, Result Error a) -> ([Interrupt], Sys.State, Result Error a)
--runTask task = case runResultant task [] of
--  (ints, Ok (st, r)) -> (ints, st, r)
--
--createTask :: Sys.State -> Sys a -> Task (Sys.State, Result Error a)
--createTask state ma = 
--  let
--    handleInterrupt :: Sys.Interrupt -> Sys.State -> Task Bool
--    handleInterrupt i _ = case i of
--      Sys.VMInterrupt x _ -> (updateState $ (:) x) >> return True
--      _                       -> return True 
--
--    runSys :: Identity a -> Task a
--    runSys ma =
--      return $ runIdentity ma
--
--  in do
--    (state', result) <- Execution.executeR
--      ma
--      runSys
--      handleInterrupt
--      state
--
--    case result of
--      Left cont     -> return (state', Err Sys.InternalError) -- DidNotComplete -- shouldn't happen
--      Right x       -> return (state', x)
--
