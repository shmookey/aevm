{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Snapshot where

import Prelude hiding (fail, putStrLn, putStr, readFile, writeFile)
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Monad (forM)
import GHC.Generics (Generic)
import Data.List.Split (chunksOf)
import Data.List (nubBy)
import Data.Map (Map)
import Data.Function (on)
import Data.ByteString (ByteString)
import System.IO (hFlush, stdout)
import qualified System.FilePath as Path
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Time.Clock as Clock

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Resultant.IO

import Fluidity.Common.RLP (RLP(..), RStruct(..))
import Fluidity.Common.Binary
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Value hiding (asAddress)
import qualified Fluidity.Common.RLP as RLP
import qualified Fluidity.EVM.Data.Prov as Prov
import qualified Fluidity.EVM.Data.ByteField as BF
import qualified Fluidity.EVM.Core.Blockchain as Blockchain


type Snap = ResultantT IO State Error
type SnapPure = Resultant () Error

instance Exceptional Error where
  fromException = IOError . show

data State = State { stDataDir :: FilePath } 

data Error
  = RLPError RLP.Error
  | IOError  String
  deriving (Show, Generic, NFData)


instance RLP AccountDB where
  toRLP (AccountDB kvs) = toRLP . map makePair $ Map.toList kvs
    where makePair (k,v) = toRLP [toRLP k, toRLP v]

  fromRLP obj = case obj of
    RList xs -> AccountDB . Map.fromList <$> mapM fromRLP xs


instance RLP Account where
  toRLP x = case x of
    Account b h s -> toRLP [toRLP $ uint b, toRLP h, toRLP s]
  
  fromRLP obj = case obj of
    RList [a, b, c] -> 
      Account <$> (valueFrom (Prov.Ext Prov.Balance) <$> fromRLP a)
              <*> fromRLP b
              <*> fromRLP c

instance RLP CodeDB where
  toRLP (CodeDB kvs) = toRLP . map makePair $ Map.toList kvs
    where makePair (k,v) = toRLP [toRLP k, toRLP $ toBytes v]

  fromRLP obj = 
    let
      fromPair :: RStruct -> Result RLP.Error (ByteString, BF.ByteField)
      fromPair obj' = case obj' of
        RList [a, b] -> do k <- fromRLP a
                           v <- fromRLP b
                           return (k, BF.fromByteString' (Prov.Ext Prov.Code) v)
    in case obj of
      RList xs -> CodeDB . Map.fromList <$> mapM fromPair xs

instance RLP StorageDB where
  toRLP (StorageDB kvvs) = 
    toRLP . Map.toList $ Map.map (toBytes . snd) kvvs

  fromRLP obj = 
    let
      fromPair :: RStruct -> Result RLP.Error (ByteString, (Value, Value))
      fromPair obj' = case obj' of
        RList [a, b] -> 
          do k <- fromRLP a
             v <- fromRLP b
             return ( k , ( valueFrom' (Prov.Ext $ Prov.Storage k v) k
                          , valueFrom' (Prov.Ext $ Prov.Storage k v) v ) )
    in case obj of
        RList xs -> StorageDB . Map.fromList <$> mapM fromPair xs


loadSnapshot :: String -> Snap Blockchain.State
loadSnapshot name = do
  accountdb <- readAccountDB name
  codedb    <- readCodeDB name
  return $ Blockchain.initState
    { Blockchain.stAccountDB = accountdb
    , Blockchain.stCodeDB    = codedb
    }

readAccountDB :: String -> Snap AccountDB
readAccountDB name = getDataDir >>= \dataDir ->
  let
    snapshotsRoot    = Path.combine dataDir "snapshots"
    snapshotDir      = Path.combine snapshotsRoot name
    pathOf           = Path.combine snapshotDir
    chunkListPath    = pathOf "ACCOUNTS"
  in do
    chunkList <- lines <$> readFile chunkListPath
    db <- forM chunkList $ \x -> do 
      chunk <- safely . B.readFile $ pathOf x
      decode chunk
    return $ mconcat db

readCodeDB :: String -> Snap CodeDB
readCodeDB name = getDataDir >>= \dataDir ->
  let
    snapshotsRoot    = Path.combine dataDir "snapshots"
    snapshotDir      = Path.combine snapshotsRoot name
    pathOf           = Path.combine snapshotDir
    chunkListPath    = pathOf "CODE"
  in do
    chunkList <- lines <$> readFile chunkListPath
    db <- forM chunkList $ \x -> do 
      chunk <- safely . B.readFile $ pathOf x
      decode chunk
    return $ mconcat db

writeAccountDB :: String -> [(ByteString, Account)] -> Snap ()
writeAccountDB name accts = getDataDir >>= \dataDir ->
  let
    snapshotsRoot    = Path.combine dataDir "snapshots"
    snapshotDir      = Path.combine snapshotsRoot name
    pathOf           = Path.combine snapshotDir
    chunkListPath    = pathOf "ACCOUNTS"

    chunkNames       = map (\i -> "accounts-" ++ show i) [0..]
    rlpChunks        = map RLP.encode $ chunksOf 4096 accts
    namedChunks      = zip chunkNames rlpChunks
    names            = map fst namedChunks

  in do
    putStrLn "Writing account database"
    forM namedChunks $ \(k, v) -> attempt ("Writing chunk " ++ k) .
      lift $ B.writeFile (pathOf k) v

    attempt "Writing chunk list" $ writeFile chunkListPath (unlines names)
    putStrLn "Finished writing account database"

writeCodeDB :: String -> [(ByteString, ByteString)] -> Snap ()
writeCodeDB name codePairs = getDataDir >>= \dataDir ->
  let
    snapshotsRoot    = Path.combine dataDir "snapshots"
    snapshotDir      = Path.combine snapshotsRoot name
    pathOf           = Path.combine snapshotDir
    chunkListPath    = pathOf "CODE"

    chunkNames       = map (\i -> "code-" ++ show i) [0..]
    rlpChunks        = map RLP.encode $ chunksOf 4096 codePairs'
    namedChunks      = zip chunkNames rlpChunks
    names            = map fst namedChunks

    codePairs'       = nubBy ((==) `on` fst) codePairs
  in do
    putStrLn "Writing code database"
    forM namedChunks $ \(k, v) -> attempt ("Writing chunk " ++ k) .
      lift $ B.writeFile (pathOf k) v

    attempt "Writing chunk list" $ writeFile chunkListPath (unlines names)
    putStrLn "Finished writing code database"

balance :: Integer -> Value
balance = valueFrom $ Prov.Ext Prov.Balance

decode :: RLP a => ByteString -> Snap a
decode = point . mapError RLPError . decodeRLP

timed :: NFData a => Snap a -> Snap (Float, a)
timed ma = do
  t1 <- lift Clock.getCurrentTime
  x  <- t1 `deepseq` ma
  t2 <- x  `deepseq` lift Clock.getCurrentTime
  t  <- t2 `deepseq` return (Clock.diffUTCTime t2 t1)
  t `deepseq` return (fromRational $ toRational t, x)


-- Monad access
-- ---------------------------------------------------------------------

getDataDir = stDataDir <$> getState

--instance RLP AccountState where
--  toRLP acct = RList
--    [ toRLP $ asAddress acct
--    , toRLP $ asBalance acct
--    , toRLP $ asCodeHash acct
--    , toRLP $ asStorage acct
--    ]
--
--  fromRLP obj = case obj of 
--    RList [a, b, c, s] ->
--      AccountState <$> fromRLP a 
--                   <*> fromRLP b
--                   <*> fromRLP c 
--                   <*> fromRLP s 
--
--instance RLP AccountDB where
--  toRLP (AccountDB kvs) = toRLP . map makePair $ Map.toList kvs
--    where makePair (k,v) =

--loadSnapshot :: String -> Maybe Int -> Bool -> Snap AccountDB
--loadSnapshot name chunkLimit noDupes = getDataDir >>= \dataDir ->
--  let
--    parseChunks :: [ByteString] -> Snap [(ByteString, Account)]
--    parseChunks xs =
--      let
--        parses = map (snd . flip runResultant () . point . mapError RLPError . RLP.decodeRLP) xs
--        tasks  = map (fmap $ concat . map toAccounts) parses
--      in
--        concat <$> mapM point (tasks `using` parList rdeepseq)
--
--    toAccounts :: AccountState -> [(ByteString, Account)]
--    toAccounts acct =
--      let
--        storageVal x  = value (roll x) (Prov.Ext Prov.Import x)
--        balanceVal x  = value x $ Prov.Ext Prov.Balance (unroll x)
--        toStorage kvs = Map.fromList $ map (\(k,v) -> (k, (storageVal k, storageVal v))) kvs
--        toAcct a b s  = (a, (Contract (balanceVal b) code $ toStorage s))
--        code          = BF.fromByteString (Prov.Ext Prov.Code (asCode acct)) $ asCode acct
--        a1            = toAcct (asAddress acct) (asBalance acct)  $ asStorage acct
--        as            = map (\(x, v, s) -> toAcct x v s) (asDupes acct)
--      in
--        a1 : as
--
--    snapshotDir   = Path.combine dataDir name
--    chunkListPath = Path.combine snapshotDir "ACCOUNTS"
--    pathOf        = Path.combine snapshotDir
--
--  in do
--    putStr "Loading blockchain snapshot... "
--
--    chunkList <- lines <$> readFile chunkListPath
--    let names  = case chunkLimit of Just n -> take n chunkList 
--                                    _      -> chunkList
--    
--    (t, x) <- timed $
--      mapM (lift . B.readFile . pathOf) names
--      >>= parseChunks
--      >>= return . Map.fromList
--
--    putStrLn $ "loaded " ++ show (Map.size x) ++ " accounts in " ++ show t ++ "s"
--    return x
--

