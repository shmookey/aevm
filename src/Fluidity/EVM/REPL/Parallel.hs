module Fluidity.EVM.REPL.Parallel where

import Prelude hiding (fail, putStrLn)
import Data.Maybe (maybe)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Time.Clock as Clock
import qualified Data.Maybe as Maybe

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (fmt, (~-), (~~))

import Confound.Methods (methodHash')
import Fluidity.Common.Binary (padBytes, unroll, toHex)
import Fluidity.EVM.REPL.Monad
import Fluidity.EVM.Types
import Fluidity.EVM.Data (Value, ByteField, formatStub, formatAddress)
import Fluidity.EVM.Blockchain (MessageCall(..), Account(..))
import Fluidity.EVM.Analyse.Outcome (PostMortem)
import Fluidity.EVM.REPL.EVM (encodeCallData)
import Fluidity.EVM.Text (formatStorage)
import qualified Fluidity.EVM.Analyse.Outcome as Outcome
import qualified Fluidity.EVM.Blockchain as Blockchain
import qualified Fluidity.EVM.Parallel as Parallel
import qualified Fluidity.EVM.Control as Control
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.Data as Data


runCommand :: Cmd.Parallel -> REPL ()
runCommand cmd = case cmd of
  Cmd.ParCall a v g d p -> call a v g d p
  Cmd.ParSets subcmd -> 
    case subcmd of
      Cmd.ParSetList          -> listSets
      Cmd.ParSetShow x        -> showSet x
      Cmd.ParSetDrop x        -> dropSet x
      Cmd.ParSetShowStorage x -> showStorage x


call :: Cmd.SetRef -> Integer -> Maybe Integer -> Maybe Cmd.CallData -> Maybe Cmd.PostProcess -> REPL ()
call ref val g cd post =
  let
    value    = Data.mkCallValue val
    gas      = Data.mkInitialGas $ Maybe.fromMaybe 1000 g
    calldata = maybe Data.fresh encodeCallData cd

    msgcall :: Address -> Address -> MessageCall
    msgcall from to = MessageCall
      { msgCaller = from
      , msgCallee = to
      , msgValue  = value
      , msgGas    = gas
      , msgData   = calldata
      }

    showResult :: (Address, PostMortem, Control.State) -> String
    showResult (addr, pm, _) =
      formatStub addr ++ " " ++ Outcome.explainPostMortem pm

    combinedFilter :: PostMortem -> Bool     
    combinedFilter = case post of 
      Just ([],_) -> const True
      Just (fs,_) -> foldl (\acc f -> \pm -> acc pm && (singleFilter f) pm) (const True) fs
      Nothing     -> const True

    singleFilter :: Cmd.Filter -> PostMortem -> Bool
    singleFilter f = case f of
      Cmd.FilterSStore  -> Outcome.pmWroteStorage
      Cmd.FilterSLoad   -> Outcome.pmReadStorage
      Cmd.FilterOk      -> Outcome.pmGracefulHalt
      Cmd.FilterErr     -> not . Outcome.pmGracefulHalt
      Cmd.FilterThrow   -> Outcome.pmProbableThrow
      Cmd.FilterCall    -> Outcome.pmMadeExtCall
      Cmd.FilterSend    -> Outcome.pmSentFunds
      Cmd.FilterNotImpl -> Outcome.pmNeedsImpl

    outputSaver :: [(Address, PostMortem, Control.State)] -> REPL ()
    outputSaver kvs = case post of
      Just (_, Just x) -> do 
        saveSet x . ParSet $ map (\(addr, _, st) -> (addr, st)) kvs
        putStrLn $ "Matching addresses saved as: " ++ x
      _ -> return ()

  in do
    putStrLn $ toHex (Data.fieldToByteString calldata)
    caller     <- getAddress
    callees    <- resolveRef ref
    msgs       <- 
      case callees of
        Left addrs -> do
          state  <- getControlState
          return $ map (\x -> (msgcall caller x, state)) addrs
        Right (ParSet entries) ->
          return $ map (\(addr, state) -> (msgcall caller addr, state)) entries

    (t, rs) <- timed . return $ Parallel.multicall msgs
    let rs'     = filter (\(_,x,_) -> combinedFilter x) rs
    mapM_ (printLn . showResult) rs'
    putStrLn $ "Completed " ++ (show $ length callees) ++ " tasks in " ++ show t ++ " seconds"
    outputSaver rs'


-- Set functions
-- ---------------------------------------------------------------------

resolveRef :: Cmd.SetRef -> REPL (Either [Address] ParSet)
resolveRef sr = case sr of
  Cmd.SetRange addr -> Left <$> matchingAddresses addr
  Cmd.SetAlias x    -> Right <$> getSet x

listSets :: REPL ()
listSets = do
  xs <- getParSets
  mapM_ (\(name, ParSet entries) -> putStrLn $ name ++ " " ++ show (length entries)) $ Map.toList xs

showSet :: String -> REPL ()
showSet x = do
  (ParSet entries) <- getSet x
  mapM_ (putStrLn . formatAddress) $ map fst entries

dropSet :: String -> REPL ()
dropSet x = updateParSets $ Map.delete x

getSet :: String -> REPL ParSet
getSet x = do
  xs <- getParSets
  case Map.lookup x xs of
    Just v  -> return v
    Nothing -> fail $ NoSuchSet x

saveSet :: String -> ParSet -> REPL ()
saveSet x ps = updateParSets $ Map.insert x ps


showStorage :: String -> REPL ()
showStorage x = 
  let
    show1 :: Address -> Control.State -> REPL ()
    show1 addr st = 
      let
        bc    = Control.stBlockchain st
        accts = Blockchain.stAccounts bc
        Just (Contract _ _ storage) = Map.lookup addr accts
     in do
       printLn $ formatAddress addr ~~ ":"
       printLn $ formatStorage storage
  in do
    (ParSet entries) <- getSet x
    mapM_ (uncurry show1) entries
  

-- Helper functions
-- ---------------------------------------------------------------------

timed :: NFData a => REPL a -> REPL (Float, a)
timed ma = do
  t1 <- io Clock.getCurrentTime
  x  <- t1 `deepseq` ma
  t2 <- x  `deepseq` io Clock.getCurrentTime
  t  <- t2 `deepseq` return (Clock.diffUTCTime t2 t1)
  t `deepseq` return (fromRational $ toRational t, x)

