module Fluidity.EVM where

import Prelude hiding (fail, putStrLn, readFile)
import Data.ByteString (ByteString)
import Data.Text.IO (putStrLn)
import System.IO.Strict (readFile)
import System.FilePath (addExtension)
import qualified System.FilePath as FilePath
import qualified Data.ByteString as B
import qualified Data.Text as T

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (Structured(fmt), typeset, withLineNumbers, (~-))

import Fluidity.Common.Binary (fromHex)
import Fluidity.EVM.REPL (REPL)
import Fluidity.EVM.Types
import qualified Fluidity.EVM.Decode as Decode
import qualified Fluidity.EVM.VM as VM
import qualified Fluidity.EVM.REPL as REPL
import qualified Fluidity.EVM.Data as Data


type EVM = ResultantT IO State Error

data State = State
  deriving (Show)

data Error
  = DecodeError Decode.Error
  | VMError     VM.Error
  | REPLError   REPL.Error
  | UnreadableBalance String String
  deriving (Show)

instance Structured Error where
  fmt e = case e of
    DecodeError x -> "Error decoding program:" ~- x
    VMError x     -> "VM halted with error:" ~- x
    REPLError x   -> "REPL error:" ~- x


-- EVM control operations
-- ---------------------------------------------------------------------

run :: EVM a -> IO (Result Error a)
run m = snd <$> runResultantT m State

runProgram :: Program -> EVM ()
runProgram = error "not implemented" -- fromVM . VM.runProgram

--load :: FilePath -> EVM Program
--load x = lift (B.readFile x) >>= decodeHex

decode :: ByteString -> EVM Code
decode = fromDecode . Decode.decode

decodeHex :: ByteString -> EVM Code
decodeHex = fromDecode . Decode.decodeHex

repl :: Maybe FilePath -> FilePath -> EVM ()
repl mdir path = 
  let
    loadAndRun :: REPL () -> EVM ()
    loadAndRun ma = do r <- lift $ REPL.runWithSetup ma
                       point $ mapError REPLError r
  in case mdir of
    Just dir -> do
      contracts <- loadFromIndex dir path
      loadAndRun $ REPL.loadMany contracts
    Nothing -> do
      (a,b,c,s) <- load "data" $ FilePath.dropExtension path
      loadAndRun $ REPL.load a b c s 


-- Loading from filesystem
-- ---------------------------------------------------------------------

load :: FilePath -> String -> EVM (Address, Balance, Bytecode, Storage)
load dir name = do
  let path = FilePath.combine dir name
  txtBytecode <- lift . readFile $ addExtension path "bin"
  txtBalance  <- lift . readFile $ addExtension path "balance"
  balance     <- let s = (readsPrec 0 txtBalance) :: [(Double, String)]
                 in case s of [(b,"")] -> return . toInteger . floor . fst $ head s
                              _        -> fail $ UnreadableBalance name txtBalance
  return 
    ( Data.mkAddress Data.Snapshot $ fromHex name
    , Data.mkBalance balance
    , fromHex txtBytecode
    , mempty
    )

loadFromIndex :: FilePath -> FilePath -> EVM ([(Address, Balance, Bytecode, Storage)])
loadFromIndex dir path = do
  names <- fmap lines . lift $ readFile path
  mapM (load dir) names


-- Formatting and display
-- ---------------------------------------------------------------------

printAsm :: Program -> EVM ()
printAsm = lift . putStrLn . typeset


-- Submodule monad adapters
-- ---------------------------------------------------------------------

fromVM :: Result VM.Error a -> EVM a
fromVM = point . mapError VMError

fromDecode :: Result Decode.Error a -> EVM a
fromDecode = point . mapError DecodeError

readInteger :: String -> Integer
readInteger = read

