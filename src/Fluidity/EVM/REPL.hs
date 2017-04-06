{-# LANGUAGE TypeSynonymInstances #-}
module Fluidity.EVM.REPL where

import Prelude hiding (Value, break, fail, print, putStr, putStrLn)
import qualified Prelude
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (elemIndex, find, intercalate)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Set ((\\))
import Data.Text (Text, pack, unpack)
import System.Console.Haskeline (InputT)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Console.Haskeline as HL
import qualified System.IO as IO

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Execution (execute)
import Text.Structured (Structured(fmt), block, typeset, (~-), (~~))
import qualified Control.Monad.Execution as Execution
import qualified Text.Structured as TS

import Confound.Methods (encodeCall)
import Fluidity.Common.Binary (unroll)
import Fluidity.EVM.Blockchain (Blockchain)
import Fluidity.EVM.Control (Control)
import Fluidity.EVM.Data (Value)
import Fluidity.EVM.Text ()
import Fluidity.EVM.Types
import Fluidity.EVM.VM (VM)
import qualified Fluidity.EVM.Blockchain as Blockchain
import qualified Fluidity.EVM.Control as Control
import qualified Fluidity.EVM.Data as Data
import qualified Fluidity.EVM.Decode as Decode
import qualified Fluidity.EVM.Provenance as Prov
import qualified Fluidity.EVM.VM as VM

import Fluidity.EVM.REPL.Monad hiding (REPL, Error)
import qualified Fluidity.EVM.REPL.Monad
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.REPL.Parser as Parser
import qualified Fluidity.EVM.REPL.Chain as REPL.Chain
import qualified Fluidity.EVM.REPL.EVM as REPL.EVM
import qualified Fluidity.EVM.REPL.Meta as REPL.Meta
import qualified Fluidity.EVM.REPL.State as REPL.State
import qualified Fluidity.EVM.REPL.Parallel as REPL.Parallel


type REPL = Fluidity.EVM.REPL.Monad.REPL
type Error = Fluidity.EVM.REPL.Monad.Error

instance Structured Error where
  fmt e = case e of
    DecodeError x       -> "Error decoding program:" ~- x
    ControlError ce     -> fmt ce
    Quit                -> fmt "Quit"
    ParseError s pe ->
      let
        pad i = take (i+s-1) $ repeat ' '
        cur i = pad i ~~ "^\n"
      in case pe of
        Parser.ParseError i x ts -> cur i ~~ x ~~ "\n" ~~ (show $ map fst ts)
        Parser.LexError i x      -> cur i ~~ x
        
    InternalError       -> fmt "Internal error."
    NoMatchingAccount x -> fmt "No matching address:" ~- (B8.unpack $ B16.encode x)
    _                   -> "Error:" ~- (show e)

instance Structured Control.Error where
  fmt err = case err of
    Control.VMError e -> fmt e
    _                 -> "Control error:" ~- show err


runREPL :: IO (Result Error ())
runREPL = HL.runInputT 
  (HL.defaultSettings { HL.historyFile = Just ".flevm-history" })
  (snd <$> runResultantT (welcome >> repl) initState)

runWithSetup :: REPL () -> IO (Result Error ())
runWithSetup pre = 
  let
    settings = HL.defaultSettings { HL.historyFile = Just ".flevm-history" }
    task     = welcome >> pre >> repl
  in
    HL.runInputT settings . fmap snd $ runResultantT task initState

loadMany :: [(Address, Balance, Bytecode, Storage)] -> REPL ()
loadMany contracts =
  let
    doImport (a,b,c,s) = Blockchain.importContract a b c s
    fst4 (a,b,c,d)     = a
    addrs              = map fst4 contracts
  in do
    mutateBlockchain $ mapM_ doImport contracts
    printLn $ "Loaded" ~- length contracts ~- "contracts"

load :: Address -> Balance -> Bytecode -> Storage -> REPL ()
load addr val code storage  = do
  mutateBlockchain $ Blockchain.importContract addr val code storage
  printStatus $ "Loaded contract at address " <> Data.formatAddress addr

repl :: REPL ()
repl = do
  lift . liftIO $ IO.hSetBuffering IO.stdout IO.NoBuffering
  void $ forever prompt

welcome :: REPL ()
welcome = do
  printLn "Fluidity EVM 0.1.0"
  printLn "Type ':help' for usage information\n"

prompt :: REPL ()
prompt = 
  let
    onError :: Error -> REPL ()
    onError = printLn . typeset
  in recoverWith onError $ do
    txt   <- getPrompt
    input <- lift $ HL.getInputLine txt
    line  <- fromMaybe Quit input
    cmd   <- point . mapError (ParseError $ length txt) $ Parser.parse line
    case cmd of
      Cmd.Chain x    -> REPL.Chain.runCommand x
      Cmd.EVM x      -> REPL.EVM.runCommand x
      Cmd.Meta x     -> REPL.Meta.runCommand x
      Cmd.State x    -> REPL.State.runCommand x
      Cmd.Parallel x -> REPL.Parallel.runCommand x


-- Printing commands
-- ---------------------------------------------------------------------

printProgramCounter :: REPL ()
printProgramCounter = queryVM VM.getPC >>= printLn . show

printStack :: REPL ()
printStack = queryVM VM.getStack >>= printLn . block . map Data.formatWord

printNextOp :: REPL ()
printNextOp = queryVM VM.nextOp >>= printLn . fst

printDisassembly :: REPL ()
printDisassembly = error ""
--  code <- queryVM VM.getCode
--  printLn $ Program code

printMemory :: REPL ()
printMemory = do
  memory <- queryVM VM.getMemory
  printLn memory

printCallData :: REPL ()
printCallData = queryVM VM.getCallData >>= printLn

printStorage :: REPL ()
printStorage = 
  let
    printPage :: (Value, Value) -> Text
    printPage (k, v) = typeset $ k ~- v
  in do
    storage <- queryVM VM.getStorage
    mapM_ putTxtLn . map printPage $ Map.toList storage

printContext :: Int -> REPL ()
printContext n = 
  let
    fmtOp :: (Int, Op) -> String
    fmtOp (i, x) = printf "%04x %s" i (unpack $ typeset x)

    noCData :: (Int, Op) -> Bool
    noCData (_, CData _) = False
    noCData _            = True

    context :: Int -> Program -> String
    context i (Program xs) =
      let 
        (l,x:r) = splitAt i (zip [0..] xs)
        l'      = reverse . take n . reverse $ filter noCData l
        r'      =           take n           $ filter noCData r
        left    = map fmtOp l'
        right   = map fmtOp r'
        x'      = fmtOp x
      in
        intercalate "\n" $ concat
          [ map ("  " ++) left
          ,     ["* " ++ x']
          , map ("  " ++) right
          ]

  in do
    pc      <- queryVM VM.getPC
    --program <- queryVM VM.getCode
    --putStrLn $ context (Data.toInt pc) (Program program)
    return ()

-- Trace commands
-- ---------------------------------------------------------------------
           
traceStack :: Int -> REPL ()
traceStack x = do
  stack <- queryVM VM.getStack
  let v = stack !! x
  printLn $ Prov.mathValue v



-- REPL display and formatting
-- ---------------------------------------------------------------------

printStatus :: String -> REPL ()
printStatus x = print "--- " >> printLn x

printError :: Error -> REPL ()
printError x = print "!!! " >> printLn (show x)


getPrompt :: REPL String
getPrompt = query Control.isInCall >>= \c ->
  if not c
  then return "Idle> "
  else do
  pc      <- queryVM VM.getPC
  stack   <- queryVM VM.getStack
  memory  <- queryVM VM.getMemory
  storage <- queryVM VM.getStorage
  (op,_)  <- queryVM VM.nextOp
  return . unpack . typeset $ 
       (TS.hexdigits 4 $ Data.toInt pc)
    ~- "|"
    ~- length stack ~~ "/"
    ~~ (Data.size memory) ~~ "/"
    ~~ Map.size storage
    ~- "|" ~- typeset op
    ~- ">>> "


-- Contract loading
-- ---------------------------------------------------------------------

decode :: ByteString -> REPL Code
decode = point . mapError DecodeError . Decode.decodeHex

