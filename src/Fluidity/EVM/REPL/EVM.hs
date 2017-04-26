module Fluidity.EVM.REPL.EVM where

import Prelude hiding (fail, putStrLn)
import Control.DeepSeq
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import Data.Time.Clock as Clock
import System.Console.ANSI
import Text.Printf (printf)
import qualified Data.List.Split as Split
import qualified Data.Maybe as M

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Resultant.IO
import Text.Structured (typeset, toString)

import Fluidity.Common.Binary
import Fluidity.Common.ANSI
import Fluidity.EVM.REPL.Monad
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField (ByteField, ByteP)
import Fluidity.EVM.Data.Format (stub)
import Fluidity.EVM.Data.Transaction (MessageCall(..))
import Fluidity.EVM.Analyse.Outcome (PostMortem)
import Fluidity.EVM.Text ()
import qualified Fluidity.EVM.Analyse.Pathfinder as Pathfinder
import qualified Fluidity.EVM.Data.ByteField as BF
import qualified Fluidity.EVM.Data.Prov as Prov
import qualified Fluidity.EVM.Analyse.Outcome as Outcome
import qualified Fluidity.EVM.Analyse.Formula as Formula
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Parallel as Parallel
import qualified Fluidity.EVM.Core.Control as Control
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.Interrupt as INT
import qualified Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Text.Disassembly as Disasm


runCommand :: Cmd.EVM -> REPL ()
runCommand cmd = case cmd of
  Cmd.Go           -> go
  Cmd.Step         -> step
  Cmd.BreakAt x    -> breakAt x
  Cmd.Paths        -> paths
  Cmd.Call a v g d -> call a v g d
  Cmd.Abort        -> abort
  Cmd.Inspect c    -> case c of
    Cmd.InspectStack      -> inspectStack
    Cmd.InspectMemory     -> inspectMemory
    Cmd.InspectStorage    -> inspectStorage
    Cmd.InspectCall       -> inspectCall
    Cmd.InspectCode r     -> inspectCode r
  Cmd.Interrupt c   -> case c of
    Cmd.InterruptOn x     -> interruptOn x
    Cmd.InterruptOff x    -> interruptOff x
    Cmd.InterruptShow     -> interruptShow
    Cmd.InterruptAction x -> interruptAction x
    Cmd.InterruptPoint x  -> interruptPoint x


go :: REPL ()
go = yield $ Control.go

step :: REPL ()
step = yield Control.step

breakAt :: Integer -> REPL ()
breakAt = yield . Control.breakAt . fromInteger

abort :: REPL ()
abort = mutate Control.abort

call :: Cmd.Address -> Value -> Value -> ByteField -> REPL ()
call addr value gas calldata = do
  caller <- getAddress
  callee <- uniqueAddress addr
  let msg = MessageCall
              { msgCaller = setProv (Prov.Usr Prov.Caller $ toBytes caller) caller
              , msgCallee = mkAddress callee
              , msgValue  = value
              , msgGas    = gas
              , msgData   = calldata
              }
  printLn msg
  yield $ Control.call msg

inspectStack :: REPL ()
inspectStack = 
  let
    showEntry :: Int -> Value -> String
    showEntry i v = " " ++ show i 
                 ++ " " ++ Format.smart v 
                 ++ " " ++ Formula.fromValue v
  in do
    stack <- zip [0..] <$> queryVM VM.getStack
    mapM_ (putStrLn . uncurry showEntry) stack

inspectMemory :: REPL ()
inspectMemory = do
  mem <- queryVM VM.getMemory
  putStrLn $ annotateByteField mem

inspectStorage :: REPL ()
inspectStorage = do
  vmState <- query Control.peek
  let callee = msgCallee $ VM.stCall vmState
  storage <- queryBlockchain . Blockchain.storage $ toBytes callee
  putStrLn (show storage)

inspectCall :: REPL ()
inspectCall = do
  vmState <- query Control.peek
  let call = VM.stCall vmState
  printLn call

inspectCode :: Maybe Cmd.CodeRef -> REPL ()
inspectCode ref = do
  code <- toBytes <$> queryVM VM.getCode
  pc   <- queryVM VM.getPC
  putStrLn $ Disasm.surroundingCode 8 8 pc code

interruptOn :: [INT.IntType] -> REPL ()
interruptOn = mapM_ (mutateSys . Sys.enableInterrupt)

interruptOff :: [INT.IntType] -> REPL ()
interruptOff = mapM_ (mutateSys . Sys.disableInterrupt)

interruptAction :: Sys.InterruptAction -> REPL ()
interruptAction = mutateSys . Sys.setInterruptAction

interruptPoint :: Sys.InterruptPoint -> REPL ()
interruptPoint = mutateSys . Sys.setInterruptPoint

paths :: REPL ()
paths = do
  st <- querySys getState
  putStrLn . Pathfinder.formatPath $ Pathfinder.tracePaths st

interruptShow :: REPL ()
interruptShow = 
  let
    showInterrupt t = case t of
      INT.IAlert  -> "alert "
      INT.ICall   -> "call  "
      INT.ICycle  -> "cycle "
      INT.IEmit   -> "emit  "
      INT.IJump   -> "jump  "
      INT.IJumpI  -> "jumpi "
      INT.IReady  -> "ready "
      INT.IReturn -> "return"
      INT.ISLoad  -> "sload "
      INT.ISStore -> "sstore"
      INT.IStop   -> "stop  "

    showIntPoint x = case x of
      Sys.Finalize  -> "finalize"
      Sys.Immediate -> "immediate"
      Sys.Preempt   -> "preempt"

    showIntAction x = case x of
      Sys.Break  -> "break"
      Sys.Echo   -> "echo"
      Sys.Ignore -> "ignore"

  in do
    flags  <- querySys $ Sys.getInterrupts
    action <- querySys $ Sys.getInterruptAction
    ipoint <- querySys $ Sys.getInterruptPoint

    putStrLn $ "Interrupt action:   " ++ showIntAction action
    putStrLn $ "Interruption point: " ++ showIntPoint ipoint
    putStrLn "Interrupt flags:"
    mapM_ putStrLn
      . map (\(k, v) -> "    " ++ showInterrupt k ++ "  " ++ show v)
      $ map (\t -> (t, INT.isEnabled t flags)) INT.intTypes

annotateByteField :: ByteField -> String
annotateByteField =
  let
    annotateWord :: Int -> ByteField -> String
    annotateWord i bf = 
      let
        byteRange  = toString . colour Cyan . (++) "0x" . toHex . padBytes 2 $ toBytes (i * 32) :: String
        byteData   = unwords . Split.chunksOf 4 $ toHex bf
        colours    = zip [0..] colouringWheel
        provGroups = filter (\x -> BF.size x > 0) $ BF.provGroups bf
        fuseGroups = map BF.fuseGroup provGroups
        groups     = zip colours provGroups
      in
        init . showWord byteRange $ foldl accGroup ("", []) groups

    showWord :: String -> (String, [String]) -> String
    showWord byteRange (bs, [])     = printf "%s  %s "        byteRange bs
    showWord byteRange (bs, p : ps) = printf "%s  %s  %s\n%s" byteRange bs p xs
      where xs  = unlines $ map (pad ++) ps
            pad = take 70 $ repeat ' '

    accGroup :: (String, [String]) -> ((Int, String -> String), ByteField) -> (String, [String])
    accGroup (acc, provs) ((n, f), bf) = (acc ++ showBytes len (n, f) bps, prv:provs)
      where len = length acc
            bps = BF.unpack bf
            prv = f $ Formula.fromValue bf

    showBytes :: Int -> (Int, String -> String) -> [BF.ByteP] -> String
    showBytes i (n, f) bps = case (i `mod` 2, bps) of
      (_, [])        -> ""
      (0, bp : bps') -> showByte f bp ++ showBytes (i+1) (n, f) bps'
      (1, bp : bps') -> showByte f bp ++ " " ++ showBytes (i+1) (n, f) bps'
      
      
    showByte :: (String -> String) -> ByteP -> String
    showByte f bp = case BF.bpProv bp of
      Prov.Nul -> toString . embolden . f $ toHex bp
      _        -> f $ toHex bp

  in
    Format.indent 2 . unlines . map (uncurry annotateWord) . zip [0..] . BF.splitWords

