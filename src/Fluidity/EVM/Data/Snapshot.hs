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


type Snap = ResultantT IO State Error
type SnapPure = Resultant () Error

instance Exceptional Error where
  fromException = IOError . show

data State = State { stDataDir :: FilePath } 

data Error
  = RLPError RLP.Error
  | IOError  String
  deriving (Show, Generic, NFData)

data AccountState = AccountState
  { asAddress    :: ByteString
  , asBalance    :: Integer
  , asCode       :: ByteString
  , asStorage    :: [(ByteString, ByteString)]
  , asDupes      :: [(ByteString, Integer, [(ByteString, ByteString)])]
  } deriving (Generic, NFData)

instance RLP AccountState where
  toRLP acct = RList
    [ toRLP $ asAddress acct
    , toRLP $ asBalance acct
    , toRLP $ asCode acct
    , toRLP $ asStorage acct
    , toRLP $ asDupes acct
    ]

  fromRLP obj = case obj of 
    RList [a, b, c, s, d] ->
      AccountState <$> fromRLP a 
                   <*> fromRLP b
                   <*> fromRLP c 
                   <*> fromRLP s 
                   <*> fromRLP d


loadSnapshot :: String -> Maybe Int -> Bool -> Snap AccountDB
loadSnapshot name chunkLimit noDupes = getDataDir >>= \dataDir ->
  let
    parseChunks :: [ByteString] -> Snap [(ByteString, Account)]
    parseChunks xs =
      let
        parses = map (snd . flip runResultant () . point . mapError RLPError . RLP.decodeRLP) xs
        tasks  = map (fmap $ concat . map toAccounts) parses
      in
        concat <$> mapM point (tasks `using` parList rdeepseq)

    toAccounts :: AccountState -> [(ByteString, Account)]
    toAccounts acct =
      let
        storageVal x  = value (roll x) (Prov.Ext Prov.Import x)
        balanceVal x  = value x $ Prov.Ext Prov.Balance (unroll x)
        toStorage kvs = Map.fromList $ map (\(k,v) -> (k, (storageVal k, storageVal v))) kvs
        toAcct a b s  = (a, (Contract (balanceVal b) code $ toStorage s))
        code          = BF.fromByteString (Prov.Ext Prov.Code (asCode acct)) $ asCode acct
        a1            = toAcct (asAddress acct) (asBalance acct)  $ asStorage acct
        as            = map (\(x, v, s) -> toAcct x v s) (asDupes acct)
      in
        a1 : as

    snapshotDir   = Path.combine dataDir name
    chunkListPath = Path.combine snapshotDir "ACCOUNTS"
    pathOf        = Path.combine snapshotDir

  in do
    putStr "Loading blockchain snapshot... "

    chunkList <- lines <$> readFile chunkListPath
    let names  = case chunkLimit of Just n -> take n chunkList 
                                    _      -> chunkList
    
    (t, x) <- timed $
      mapM (lift . B.readFile . pathOf) names
      >>= parseChunks
      >>= return . Map.fromList

    putStrLn $ "loaded " ++ show (Map.size x) ++ " accounts in " ++ show t ++ "s"
    return x

writeSnapshot :: String -> [AccountState] -> Snap ()
writeSnapshot name accts = getDataDir >>= \dataDir ->
  let
    accChunks :: Int -> ([AccountState], [[AccountState]], Int) -> AccountState -> ([AccountState], [[AccountState]], Int)
    accChunks sz (chunk, chunks, n) acct =
      let
        m = length (asDupes acct) + 1
      in if n + m < sz
         then (acct:chunk, chunks, n + m)
         else ([acct], chunk:chunks, m)

    snapshotsRoot    = Path.combine dataDir "snapshots"
    snapshotDir      = Path.combine snapshotsRoot name
    pathOf           = Path.combine snapshotDir
    chunkListPath    = pathOf "ACCOUNTS"

    (final, rest, _) = foldl (accChunks 8192) ([], [], 0) accts
    chunkNames       = map (\i -> "accounts-" ++ show i) [0..]
    rlpChunks        = map RLP.encode $ final : rest
    namedChunks      = zip chunkNames rlpChunks
    names            = map fst namedChunks

  in do
    forM namedChunks $ \(k, xs) ->
      attempt ("Writing chunk " ++ name) (lift $ B.writeFile (pathOf k) xs)

    attempt "Writing chunk list" $ writeFile chunkListPath (unlines names)
    putStrLn "Finished writing snapshot"

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

