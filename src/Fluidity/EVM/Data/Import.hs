module Fluidity.EVM.Data.Import where

import Prelude hiding (fail)
import Data.Map (Map)
import Data.List (sortOn, groupBy)
import Data.Function (on)
import Data.ByteString (ByteString)
import qualified System.FilePath as FilePath
import qualified Codec.Compression.Snappy as Snappy
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import Control.Monad.Result
import Control.Monad.Resultant

import Fluidity.Common.RLP (RStruct(..))
import Fluidity.Common.Binary
import Fluidity.Common.Crypto
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Snapshot hiding (State, loadChunkData, parseStateChunk)
import qualified Fluidity.Common.RLP as RLP
import qualified Fluidity.EVM.Data.Snapshot as Snapshot


type Import = ResultantT IO State RLPError

data State = State
  { stSrcDir            :: FilePath
  , stDstDir            :: FilePath
  , stAddrMatchFailures :: Int 
  }

-- | Pure version, for parallel processing of state chunks
type ImportPure = Resultant Int RLPError

data Manifest = Manifest
  { mVersion     :: Integer
  , mStateHashes :: [ByteString]
  , mBlockHashes :: [ByteString]
  , mStateRoot   :: ByteString
  , mBlockNumber :: Integer
  , mBlockHash   :: ByteString
  }

type StateChunk = [RichAccount]

data RichAccount = RichAccount
  { raAddrHash :: ByteString
  , raNonce    :: Integer
  , raBalance  :: Integer
  , raCodeFlag :: Integer
  , raCode     :: ByteString
  , raStorage  :: StorageState
  }

-- Intermediate data types: partially processed chunk data

type SplitChunk = 
  ( [CodeHashPair]  -- Contracts with original code, and the hash of that code
  , [CodeHashPair]  -- Contracts with duplicate code, and the hash of the original
  )

type CodeHashPair = (RichAccount, ByteString)
type DupeEntry = (ByteString, Integer, StorageState)
type CodeGroup = (RichAccount, [RichAccount])
type HashGroup = (ByteString, [RichAccount])


importSnapshot :: Import ()
importSnapshot = do
  lift $ putStrLn "Importing blockchain snapshot. This may take a while."
  manifest    <- loadManifest
  addrmap     <- loadAddressMap
  rawchunks   <- loadChunkData manifest
  lift $ putStrLn "Finished loading chunk data" 
  (_, statechunks) <- runPure $ mapM (uncurry parseStateChunk) rawchunks
  lift $ putStrLn "Finished parsing state chunks"
  (_, codegroups) <- runPure $ extractCodeGroups (map splitChunk statechunks)
  lift . putStrLn $ "Number of code groups: " ++ show (length codegroups)
  (n, accts)       <- runPure $ convert statechunks addrmap
  lift . putStrLn $ show n ++ " address match failures"
  lift $ putStrLn "Writing snapshot"
  runSnap $ writeSnapshot accts
  lift $ putStrLn "Import complete."

runPure :: ImportPure a -> Import (Int, a)
runPure ma = do
  let (n, r) = runResultant ma 0
  point $ fmap (\x -> (n,x)) r

runSnap :: Snap a -> Import a
runSnap ma = do
  dir <- getDstDir
  let st = Snapshot.State dir Nothing False
  r <- lift (runResultantT ma st)
  point $ snd r

convert :: [StateChunk] -> Map ByteString ByteString -> ImportPure [AccountState]
convert chunks addrmap = 
  extractCodeGroups (map splitChunk chunks) >>= mapM (mkAccountState addrmap)

mkAccountState :: Map ByteString ByteString -> CodeGroup -> ImportPure AccountState
mkAccountState addrmap (acct, dupes) =
  let
    mkDupe :: RichAccount -> ImportPure (ByteString, Integer, [(ByteString, ByteString)])
    mkDupe x = getAddr (raAddrHash x) >>= \a -> return (a, raBalance x, raStorage x)

    defaultAddr :: ByteString -> RLPError -> ImportPure ByteString
    defaultAddr bs _ = updateState (+1) >> return bs

    getAddr :: ByteString -> ImportPure ByteString
    getAddr x = recoverWith (defaultAddr x) (fromMaybe (NoSuchAddress $ toHex x) $ Map.lookup x addrmap)

  in do
    addr <- getAddr (raAddrHash acct)
    dupes' <- mapM mkDupe dupes
    return AccountState
      { asAddress = addr
      , asBalance = raBalance acct
      , asCode    = raCode acct
      , asStorage = raStorage acct
      , asDupes   = dupes'
      }

-- Group each unique contract with its duplicates
extractCodeGroups :: [SplitChunk] -> ImportPure [CodeGroup]
extractCodeGroups chunks = 
  let
    (uniques, dupes) = uncurry ((,) `on` (sortOn snd . concat)) $ unzip chunks

    dupeGroups :: [HashGroup]
    dupeGroups = map trimKeys $ groupBy ((==) `on` snd) dupes

    trimKeys :: [CodeHashPair] -> HashGroup
    trimKeys ((x,k):xks) = (k, x:(map fst xks))
 
    -- Tail-recursive algorithm to Work through the sorted groups to find matches
    rejoin :: [CodeHashPair] -> [HashGroup] -> [CodeGroup] -> ImportPure [CodeGroup]
    rejoin uniqs dgrps acc = case (uniqs, dgrps) of
      ([], [] ) -> return acc                                    -- no more uniques or dupes
      ([], _:_) -> fail $ IntegrityError "unmatched dupes"       -- no more unqiues, but dupes remain
      (_ , [] ) -> return $ acc ++ map (flip (,) [] . fst) uniqs -- no more dupes
      ((acct, uhash):uniqs', (ghash, accts):dgrps') ->           -- accounts remain
        if
          uhash == ghash
        then
          rejoin uniqs' dgrps' $ (acct, accts) : acc
        else if
          uhash < ghash
        then
          rejoin uniqs' dgrps $ (acct, []) : acc
        else
          fail $ IntegrityError "unmatched low key dupes"
  in
    rejoin uniques dupeGroups []

-- Split a chunk into unique and duplicate contracts, we can do this bit in parallel
splitChunk :: StateChunk -> SplitChunk
splitChunk accts = 
  let
    addNext :: SplitChunk -> RichAccount -> SplitChunk
    addNext (uniques, dupes) acct = case (raCodeFlag acct, raCode acct) of
      (0, _   ) -> (uniques, dupes)
      (1, code) -> ((acct, keccak256 code):uniques, dupes)
      (2, code) -> (uniques, (acct, code):dupes)

    (uniques, dupes) = foldl addNext ([],[]) accts
  in
    (sortOn snd uniques, sortOn snd dupes)

loadManifest :: Import Manifest
loadManifest = 
  let
    parseManifest :: [RStruct] -> Import Manifest
    parseManifest keys = case keys of
      [RItem v, shs, bhs, RItem sr, RItem bn, RItem bh] -> do
        stateHashes <- rlpList shs >>= mapM rlpItem
        blockHashes <- rlpList bhs >>= mapM rlpItem
        return Manifest
          { mVersion     = roll v
          , mStateHashes = stateHashes
          , mBlockHashes = blockHashes
          , mStateRoot   = sr
          , mBlockNumber = roll bn
          , mBlockHash   = bh
          }
      _ -> fail $ ManifestParseError "Unexpected manifest structure"
  in do
    lift $ putStrLn "Reading manifest"
    dir <- getSrcDir
    rlp <- readFileRLP $ FilePath.combine dir "MANIFEST"
    rlpList rlp >>= parseManifest

loadChunkData :: Manifest -> Import [(String, ByteString)]
loadChunkData (Manifest {mStateHashes = chunkNames }) = do
  lift $ putStrLn "Loading state chunks from disk"
  dir <- getSrcDir
  let names = map toHex chunkNames
  chunks <- lift $ mapM (B.readFile . FilePath.combine dir) names
  return $ zip names chunks

loadAddressMap :: Import (Map ByteString ByteString)
loadAddressMap = do
  dir   <- getSrcDir
  addrs <- B8.lines <$> (lift . B.readFile $ FilePath.combine dir "ADDRESSES")
  lift . putStrLn $ "Loaded address map (" ++ show (length addrs) ++ " addresses)"
  return . Map.fromList . map (\a -> (keccak256 a, a)) $ map fromHex addrs

dumpChunk :: String -> ByteString -> Import ()
dumpChunk name bs = lift $ B.writeFile (".aevm/chunk-" ++ name ++ ".bin") (Snappy.decompress bs)

parseStateChunk :: String -> ByteString -> ImportPure StateChunk
parseStateChunk name bs =
  let
    parseRichAccount :: RStruct -> ImportPure RichAccount
    parseRichAccount obj = case obj of
      RList [a, RList [n, b, f, c, s]] -> do
        addr    <- rlpItem a
        nonce   <- rlpItem n
        balance <- rlpItem b
        flag    <- rlpItem f
        code    <- rlpItem c
        storage <- parseStorage s
        return RichAccount
          { raAddrHash = addr
          , raNonce    = roll nonce
          , raBalance  = roll balance
          , raCodeFlag = roll flag
          , raCode     = code
          , raStorage  = storage
          }
      _ -> fail $ StateParseError "Unexpected account structure"
  in do
    rlpDeserialise name (Snappy.decompress bs) >>= rlpList >>= mapM parseRichAccount


getSrcDir = stSrcDir <$> getState
getDstDir = stDstDir <$> getState
getAddrMatchFailures = stAddrMatchFailures <$> getState

setAddrMatchFailures x = updateState (\st -> st { stAddrMatchFailures = x }) :: Import ()

updateAddrMatchFailures f = getAddrMatchFailures >>= setAddrMatchFailures . f
{-

Manifest:
[
    version: P, // snapshot format version. Must be set to 2.
    state_hashes: [hash_1: B_32, hash_2: B_32, ...], // a list of all the state chunks in this snapshot
    block_hashes: [hash_1: B_32, hash_2: B_32, ...], // a list of all the block chunks in this snapshot
    state_root: B_32, // the root which the rebuilt state trie should have. used to ensure validity
    block_number: P, // the number of the best block in the snapshot; the one which the state coordinates to.
    block_hash: B_32, // the best block in the snapshot's hash.
]

State chunks:
[ [hash1: B_32, acc_1: P], [hash_2: B_32, acc_2: P], ... ]

Rich account:
[
    nonce: B_32,
    balance: B_32,
    code_flag: B_1,
    code: P,
    storage: [[keyhash1: B_32, val1: B_32], [keyhash2: B_32, val2: B_32], ...]
]

Block chunks:
[
    number: P, // number of the first block in the chunk
    hash: B_32, // hash of the first block in the chunk
    td: B_32, // total difficulty of the first block in the chunk
    [abridged_1: AB, receipts_1: RC], // The abridged RLP for the first block, and its receipts.
    [abridged_2: AB, receipts_2: RC], // The abridged RLP for the second block, and its receipts.
    [abridged_3: AB, receipts_3: RC], // ... and so on.
    ...
]

Abridged block RLP:
[
    // some header fields
    author: B_20,
    state_root: B_32,
    log_bloom: B_256,
    difficulty: B_32,
    gas_limit: B_32,
    gas_used: B_32,
    timestamp: P,
    extra_data: P,

    // uncles and transactions inline
    [tx_1: P, tx_2: P, ...],
    [uncle_1: P, uncle_2: P, ...],

    // Seal fields
    mix_hash: B_32,
    nonce: B_8,
]

-}
