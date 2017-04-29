module Fluidity.EVM.REPL.EVM where

import Prelude hiding (fail, putStr, putStrLn)
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
import qualified Fluidity.EVM.Core.Interrupt as I
import qualified Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Text.Disassembly as Disasm


runCommand :: Cmd.EVM -> REPL ()
runCommand cmd = case cmd of
  Cmd.Go            -> go
  Cmd.Step          -> step
  Cmd.BreakAt x     -> breakAt x
  Cmd.Paths         -> paths
  Cmd.Call a v g d  -> call a v g d
  Cmd.Enter a v g d -> enter a v g d
  Cmd.Abort         -> abort
  Cmd.Show c     -> case c of
    Cmd.ShowStack      -> showStack
    Cmd.ShowMemory     -> showMemory
    Cmd.ShowStorage    -> showStorage
    Cmd.ShowCall       -> showCall
    Cmd.ShowCode r     -> showCode r
  Cmd.Interrupt c   -> case c of
    Cmd.InterruptBreak x    -> interruptBreak x
    Cmd.InterruptEcho x     -> interruptEcho x
    Cmd.InterruptOff x      -> interruptOff x
    Cmd.InterruptShow       -> interruptShow
    Cmd.InterruptStrategy x -> interruptStrategy x


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

enter :: Cmd.Address -> Value -> Value -> ByteField -> REPL ()
enter addr value gas calldata = do
  ints <- querySys Sys.getInterrupts
  bracket
    (mutateSys $ Sys.setInterruptAction I.Break I.IReady)
    (mutateSys $ Sys.setInterrupts ints)
    (call addr value gas calldata)

showStack :: REPL ()
showStack = 
  let
    showEntry :: Int -> Value -> String
    showEntry i v = " " ++ show i 
                 ++ " " ++ Format.smart v 
                 ++ " " ++ Formula.fromValue v
  in do
    stack <- zip [0..] <$> queryVM VM.getStack
    mapM_ (putStrLn . uncurry showEntry) stack

showMemory :: REPL ()
showMemory = do
  mem <- queryVM VM.getMemory
  putStrLn $ annotateByteField mem

showStorage :: REPL ()
showStorage = do
  vmState <- query Control.peek
  let callee = msgCallee $ VM.stCall vmState
  storage <- queryBlockchain . Blockchain.storage $ toBytes callee
  putStrLn (show storage)

showCall :: REPL ()
showCall = do
  vmState <- query Control.peek
  let call = VM.stCall vmState
  printLn call

showCode :: Maybe Cmd.Slice -> REPL ()
showCode ref =
  let 
    (pre, post) = M.fromMaybe (8,8) $ fmap (readSlice 8 8) ref
  in do
    code <- toBytes <$> queryVM VM.getCode
    pc   <- queryVM VM.getPC
    putStr $ Disasm.surroundingCode pre post pc code

readSlice :: Int -> Int -> Cmd.Slice -> (Int, Int)
readSlice a b ab = (M.fromMaybe a $ fst ab, M.fromMaybe b $ snd ab)

interruptBreak :: [I.IntType] -> REPL ()
interruptBreak = mapM_ (mutateSys . Sys.setInterruptAction I.Break)

interruptEcho :: [I.IntType] -> REPL ()
interruptEcho = mapM_ (mutateSys . Sys.setInterruptAction I.Echo)

interruptOff :: [I.IntType] -> REPL ()
interruptOff = mapM_ (mutateSys . Sys.setInterruptAction I.Ignore)

interruptStrategy :: Sys.Strategy -> REPL ()
interruptStrategy = mutateSys . Sys.setStrategy

paths :: REPL ()
paths = do
  st <- querySys getState
  putStrLn . Pathfinder.formatPath $ Pathfinder.tracePaths st

interruptShow :: REPL ()
interruptShow = 
  let
    showInterrupt t = case t of
      I.IAlert  -> "alert "
      I.ICall   -> "call  "
      I.ICycle  -> "cycle "
      I.IEmit   -> "emit  "
      I.IJump   -> "jump  "
      I.IJumpI  -> "jumpi "
      I.IReady  -> "ready "
      I.IReturn -> "return"
      I.ISLoad  -> "sload "
      I.ISStore -> "sstore"
      I.IStop   -> "stop  "

    showIntStrategy x = case x of
      Sys.Wait      -> "wait"
      Sys.Immediate -> "immediate"
      Sys.Preempt   -> "preempt"

    showIntAction x = case x of
      I.Break  -> "break"
      I.Echo   -> "echo"
      I.Ignore -> "ignore"

  in do
    flags  <- querySys $ Sys.getInterrupts
    strat  <- querySys $ Sys.getStrategy

    putStrLn $ "Interruption strategy: " ++ showIntStrategy strat
    putStrLn "Interrupt flags:"
    mapM_ putStrLn
      . map (\(k, v) -> "    " ++ showInterrupt k ++ "  " ++ showIntAction v)
      $ map (\t -> (t, I.getAction t flags)) I.types

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

