{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Snapshot where

import Prelude hiding (fail)
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Function (on)
import Data.ByteString (ByteString)
import System.IO (hFlush, stdout)
import qualified System.FilePath as FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Time.Clock as Clock

import Control.Monad.Result
import Control.Monad.Resultant

import Fluidity.Common.RLP (RLP(toRLP), RStruct(..))
import Fluidity.Common.Binary
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Value hiding (asAddress)
import qualified Fluidity.Common.RLP as RLP
import qualified Fluidity.EVM.Data.Prov as Prov
import qualified Fluidity.EVM.Data.ByteField as BF


type Snap = ResultantT IO State RLPError
type SnapPure = Resultant () RLPError

data State = State
  { stDir       :: FilePath
  , stMaxChunks :: Maybe Int
  , stNoDupes   :: Bool
  } 

data RLPError
  = ManifestParseError String
  | StateParseError String
  | RLPError String String
  | IntegrityError String
  | NoSuchAddress String
  deriving (Show, Generic, NFData)


data AccountState = AccountState
  { asAddress    :: ByteString
  , asBalance    :: Integer
  , asCode       :: ByteString
  , asStorage    :: [(ByteString, ByteString)]
  , asDupes      :: [(ByteString, Integer, [(ByteString, ByteString)])]
  } deriving (Generic, NFData)

type Dupe         = (ByteString, Integer, StorageState)
type StorageState = [(ByteString, ByteString)]
type StorageSlot  = (ByteString, ByteString)

instance RLP StorageSlot where
  toRLP (k, v) = toRLP [k, v]

instance RLP Dupe where
  toRLP (k, v, s) = RList [toRLP k, toRLP v, toRLP s]

instance RLP Integer where
  toRLP x = if x == 0 then toRLP (B.singleton 0) else toRLP $ unroll x

instance RLP AccountState where
  toRLP acct =
    let
      rAddress = toRLP $ asAddress acct
      rBalance = toRLP $ asBalance acct
      rCode    = toRLP $ asCode acct
      rStorage = toRLP $ asStorage acct
      rDupes   = toRLP $ asDupes acct
    in
      RList [rAddress, rBalance, rCode, rStorage, rDupes]


loadSnapshot :: Snap AccountDB
loadSnapshot = 
  let
    parseChunks :: Bool -> [ByteString] -> Snap [(ByteString, Account)]
    parseChunks noDupes xs =
      let
        parses :: [Result RLPError [AccountState]]
        parses = map (snd . flip runResultant () . parseStateChunk noDupes) xs
        
        tasks :: [Result RLPError [(ByteString, Account)]]
        tasks = map (fmap $ concat . map toAccounts) parses
      in
        concat <$> mapM point (tasks `using` parList rdeepseq)

  in do
    lift $ putStr "Loading blockchain snapshot... "
    lift $ hFlush stdout

    dir       <- getDir
    maxChunks <- getMaxChunks
    noDupes   <- getNoDupes

    (t, x) <- timed $ do
      chunkList <- lines <$> lift (readFile $ FilePath.combine dir "ACCOUNTS")
      let names  = case maxChunks of { Just n -> take n chunkList ; _ -> chunkList }
      chunks    <- lift $ mapM (B.readFile . FilePath.combine dir) names
      accounts  <- parseChunks noDupes chunks
      return $ Map.fromList accounts

    lift . putStrLn $ "loaded " ++ show (Map.size x) ++ " accounts in " ++ show t ++ "s"
    return x

toAccounts :: AccountState -> [(ByteString, Account)]
toAccounts acct =
  let
    storageVal :: ByteString -> Value
    storageVal x = value (roll x) (Prov.Ext Prov.Import x)

    balanceVal :: Integer -> Value
    balanceVal x = value x $ Prov.Ext Prov.Balance (unroll x)

    toStorage :: [(ByteString, ByteString)] -> StorageDB
    toStorage kvs =
      Map.fromList $ map (\(k,v) -> (k, (storageVal k, storageVal v))) kvs

    toAcct a b s = (a, (Contract (balanceVal b) code $ toStorage s))

    code = BF.fromByteString (Prov.Ext Prov.Code (asCode acct)) $ asCode acct
    a1   = toAcct (asAddress acct) (asBalance acct)  $ asStorage acct
    as   = map (\(x, v, s) -> toAcct x v s) (asDupes acct)
  in
    a1 : as

writeSnapshot :: [AccountState] -> Snap ()
writeSnapshot accts =
  let
    count :: AccountState -> Int
    count acct = length (asDupes acct) + 1

    accChunks :: Int -> ([AccountState], [[AccountState]], Int) -> AccountState -> ([AccountState], [[AccountState]], Int)
    accChunks sz (chunk, chunks, n) acct =
      let
        m = count acct
      in if n + m < sz
         then (acct:chunk, chunks, n + m)
         else ([acct], chunk:chunks, m)

    writeChunk :: FilePath -> String -> [AccountState] -> Snap ()
    writeChunk dir name accts = do
      lift . putStrLn $ "Writing chunk " ++ name
      lift . B.writeFile (FilePath.combine dir name) $ RLP.encode accts

    writeChunkList :: FilePath -> Snap ()
    writeChunkList dir = do
      lift . putStrLn $ "Writing chunk list"
      lift . writeFile (FilePath.combine dir "ACCOUNTS") $ unlines (map fst namedChunks)

    (finalChunk, restChunks, _) = foldl (accChunks 8192) ([], [], 0) accts
    chunkNames = map (\i -> "accounts-" ++ show i) [0..]
    namedChunks = zip chunkNames $ finalChunk : restChunks

  in do
    dir <- getDir
    mapM (uncurry $ writeChunk dir) namedChunks
    writeChunkList dir
    lift $ putStrLn "Finished writing snapshot"

parseStateChunk :: Bool -> ByteString -> SnapPure [AccountState]
parseStateChunk noDupes bs =
  let
    parseAccountState :: RStruct -> SnapPure AccountState
    parseAccountState keys = case keys of
      RList [RItem a, RItem b, RItem c, s, RList ds] -> do
        storage <- parseStorage s
        dupes   <- if noDupes then return [] else mapM parseDupe ds
        return AccountState
          { asAddress  = a
          , asBalance  = roll b
          , asCode     = c
          , asStorage  = storage
          , asDupes    = dupes
          }
      _ -> fail $ StateParseError "Unexpected account structure"

    parseDupe :: RStruct -> SnapPure Dupe
    parseDupe keys = case keys of
      RList [RItem k, RItem v, s] -> do
        storage <- parseStorage s
        return (k, roll v, storage)

  in do
    entries <- rlpDeserialise ".aevm/snapshot" bs >>= rlpList
    mapM parseAccountState entries


both :: Monad m => (m a, m b) -> m (a, b)
both (ma, mb) = do { a <- ma ; b <- mb ; return (a, b) }


-- | These are defined in Rise because they're used by four monads: Snap, SnapPure, Import, ImportPure

parseStorage :: Rise r i s RLPError => RStruct -> r i s RLPError StorageState
parseStorage obj =
  rlpList obj >>= mapM rlpTuple >>= mapM (both . uncurry (on (,) rlpItem))

rlpTuple :: Rise r i s RLPError => RStruct -> r i s RLPError (RStruct, RStruct)
rlpTuple obj = case obj of
  RList [a, b] -> return (a, b)
  RList []     -> return (RItem mempty, RItem mempty)
  RList xs     -> fail . RLPError "" $ "Failed to parse tuple. Too many elements: " ++ show (length xs)
  _            -> fail $ RLPError "" "Failed to parse tuple. Not a list."

readFileRLP :: Rise r IO s RLPError => FilePath -> r IO s RLPError RStruct
readFileRLP x = do
  bs <- lift $ B.readFile x
  recoverWith (\e -> fail . RLPError "" $ "Error reading " ++ x ++ ": " ++ show e) (rlpDeserialise "" bs)
  

rlpDeserialise :: Rise r i s RLPError => String -> ByteString -> r i s RLPError RStruct
rlpDeserialise name bs = case RLP.decode bs of
  Ok obj -> return obj
  Err e  -> fail . RLPError name $ "RLP deserialisation error: " ++ show e

rlpItem :: Rise r i s RLPError => RStruct -> r i s RLPError ByteString
rlpItem obj = case obj of
  RItem x -> return x
  _         -> fail $ RLPError "" "Unexpected RLP list, expecting item"

rlpList :: Rise r i s RLPError => RStruct -> r i s RLPError [RStruct]
rlpList obj = case obj of
  RList xs -> return xs
  _          -> fail $ RLPError "" "Unexpected RLP item, expecting list"


timed :: NFData a => Snap a -> Snap (Float, a)
timed ma = do
  t1 <- lift Clock.getCurrentTime
  x  <- t1 `deepseq` ma
  t2 <- x  `deepseq` lift Clock.getCurrentTime
  t  <- t2 `deepseq` return (Clock.diffUTCTime t2 t1)
  t `deepseq` return (fromRational $ toRational t, x)


-- Monad access
-- ---------------------------------------------------------------------

getDir       = stDir       <$> getState
getMaxChunks = stMaxChunks <$> getState
getNoDupes   = stNoDupes   <$> getState

