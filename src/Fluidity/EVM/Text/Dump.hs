{- | Pretty-printer for internal state records -}
module Fluidity.EVM.Text.Dump where

import Data.List (intercalate)

import Fluidity.Common.Binary (toHex)

import Fluidity.EVM.Data.Value
import Fluidity.EVM.Text.Util
import qualified Fluidity.EVM.Data.Format as F
import qualified Fluidity.EVM.Data.ByteField as BF
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.Blockchain as Chain
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.Interrupt as I
import qualified Fluidity.EVM.Data.Account as Account
import qualified Fluidity.EVM.Data.Transaction as Tx


class Dump a where
  dumpFull    :: a -> String -- Full recursive dump
  dumpShallow :: a -> String -- Always collapse substates to one line
  dumpCompact :: a -> String -- Sometimes collapse substates to one line
  dumpLine    :: a -> String -- Summarise in one line

instance Dump a => Dump [a] where
  dumpFull xs    = dumpList dumpFull xs
  dumpShallow xs = dumpList dumpLine xs
  dumpCompact xs = dumpList dumpShallow xs
  dumpLine xs    = "(" ++ show (length xs) ++ " entries)"

dumpList :: (a -> String) -> [a] -> String
dumpList f xs = 
  let
    arrElem c x = c : (drop 1 . indent 2 $ f x)
  in case xs of
    []      -> "[]"
    x : []  -> arrElem '[' x ++ "]"
    x : xs' -> arrElem '[' x ++ concatMap (arrElem ',') xs' ++ "]"

easyLine :: Show a => a -> String
easyLine = take 60 . concat . lines . show


-- System
-- ---------------------------------------------------------------------

instance Dump Sys.State where
  dumpFull x = unlines $ map trimRight 
     [ "Status: " ++ showSysStatus (Sys.stStatus x)
     , "Config:"
     ,   indent 2 (dumpFull $ Sys.stConfig x)
     , "Stack:"
     ,   indent 2 (dumpFull $ Sys.stStack x)
     , "Chain:" 
     ,   indent 2 (dumpFull $ Sys.stChain x)
     ]
  dumpShallow x = unlines $ map trimRight
     [ "Status: " ++ showSysStatus (Sys.stStatus x)
     , "Config:"
     ,   indent 2 (dumpShallow $ Sys.stConfig x)
     , "Stack:"
     ,   indent 2 (dumpShallow $ Sys.stStack x)
     , "Chain:" 
     ,   indent 2 (dumpShallow $ Sys.stChain x)
     ]
  dumpCompact x = unlines $ map trimRight
     [ "Status: " ++ showSysStatus (Sys.stStatus x)
     , "Config:"
     ,   indent 2 (dumpShallow $ Sys.stConfig x)
     , "Stack:"
     ,   indent 2 (dumpCompact $ Sys.stStack x)
     , "Chain:" 
     ,   indent 2 (dumpShallow $ Sys.stChain x)
     ]
  dumpLine x = "<System: " ++ show (length $ Sys.stStack x) ++ " active calls>"

instance Dump Sys.Config where
  dumpFull x = unlines $ map trimRight
     [ "Strategy:    " ++ show (Sys.cStrategy x)
     , "Interrupts:  " ++ dumpFull (Sys.cInterrupts x)
     , "Breakpoints: " ++ show (Sys.cBreakpoints x)
     ]
  dumpShallow x = unlines $ map trimRight
     [ "Strategy:    " ++ show (Sys.cStrategy x)
     , "Interrupts:  " ++ dumpLine (Sys.cInterrupts x)
     , "Breakpoints: " ++ show (Sys.cBreakpoints x)
     ]
  dumpCompact x = unlines $ map trimRight
     [ "Strategy:    " ++ show (Sys.cStrategy x)
     , "Interrupts:  " ++ dumpLine (Sys.cInterrupts x)
     , "Breakpoints: " ++ show (Sys.cBreakpoints x)
    ]
  dumpLine x = show x

showSysStatus :: Sys.Status -> String
showSysStatus s = case s of
  Sys.Running         -> "Running"
  Sys.Preempted       -> "Preempted"
  Sys.Waiting _ _     -> "Waiting"
  Sys.Preemptible _ _ -> "Preemptible"
 

-- VM
-- ---------------------------------------------------------------------

instance Dump VM.State where
  dumpFull x = unlines $ map trimRight
     [ "Status: " ++ show  (VM.stStatus x)
     , "PC:     " ++ show  (VM.stPC x)
     , "Code:   " ++ toHex (VM.stCode x)
     , "Memory: " ++ toHex (VM.stMemory x)
     , "Gas:    " ++ show  (VM.stGas x)
     , "Stack:"
     ,   indent 2 (dumpFull $ VM.stStack x)
     , "Call:"
     ,   indent 2 (dumpFull $ VM.stCall x)
     ]
  dumpShallow x = unlines $ map trimRight
     [ "Status: " ++ show  (VM.stStatus x)
     , "PC:     " ++ show  (VM.stPC x)
     , "Code:   " ++ "(" ++ show (BF.size $ VM.stCode x) ++ " bytes)"
     , "Stack:  " ++ "[" ++ intercalate ", "  (map F.smart $ VM.stStack x) ++ "]"
     , "Memory: " ++ toHex (VM.stMemory x)
     , "Gas:    " ++ show  (VM.stGas x)
     , "Call:   " ++ dumpLine (VM.stCall x)
     ]
  dumpCompact = dumpShallow
  dumpLine  x = "<VM: PC=" ++ show (VM.stPC x) ++ ">"


-- Blockchain
-- ---------------------------------------------------------------------

instance Dump Chain.State where
  dumpFull x = unlines $ map trimRight
     [ "CurrentBlock:" 
     ,   indent 2 (dumpFull $ Chain.stCurrentBlock x)
     , "AccountDB:"
     ,   indent 2 (dumpFull $ Chain.stAccountDB x)
     , "CodeDB:"
     ,   indent 2 (dumpFull $ Chain.stCodeDB x)
     , "Blocks:" 
     ,   indent 2 (dumpFull $ Chain.stBlocks x)
     ]
  dumpShallow x = unlines $ map trimRight
     [ "CurrentBlock: " ++ dumpLine (Chain.stCurrentBlock x)
     , "AccountDB:    " ++ dumpLine (Chain.stAccountDB x)
     , "CodeDB:       " ++ dumpLine (Chain.stCodeDB x)
     , "Blocks:       " ++ dumpLine (Chain.stBlocks x)
     ]
  dumpCompact x = unlines $ map trimRight
     [ "AccountDB:    " ++ dumpLine (Chain.stAccountDB x)
     , "CodeDB:       " ++ dumpLine (Chain.stCodeDB x)
     , "Blocks:       " ++ dumpLine (Chain.stBlocks x)
     , "CurrentBlock:" 
     ,   indent 2 (dumpFull $ Chain.stCurrentBlock x)
     ]
  dumpLine x = 
    let n = int . Chain.blkNumber $ Chain.stCurrentBlock x
    in "<Blockchain: Block #" ++ show n ++ ">"

instance Dump Chain.Block where
  dumpFull x = unlines $ map trimRight
     [ "Number:       " ++ F.smart (Chain.blkNumber x)
     , "Hash:         " ++ F.smart (Chain.blkHash x)
     , "Time:         " ++ F.smart (Chain.blkTime x) 
     , "GasPrice:     " ++ F.smart (Chain.blkGasPrice x)
     , "Difficulty:   " ++ F.smart (Chain.blkDifficulty x)
     , "Coinbase:     " ++ F.smart (Chain.blkCoinbase x)
     , "Transactions: " ++ show  (Chain.blkTransactions x)
     ]
  dumpShallow = dumpFull
  dumpCompact = dumpFull
  dumpLine  x = "<Block: #" ++ show (int $ Chain.blkNumber x) ++ ">"


-- Other
-- ---------------------------------------------------------------------

instance Dump Tx.MessageCall where
  dumpFull x = unlines $ map trimRight
     [ "Caller:       " ++ F.smart (Tx.msgCaller x)
     , "Callee:       " ++ F.smart (Tx.msgCallee x)
     , "Value:        " ++ F.smart (Tx.msgValue x) 
     , "Gas:          " ++ F.smart (Tx.msgGas x)
     , "Data:         " ++ toHex   (Tx.msgData x)
     ]
  dumpShallow = dumpFull
  dumpCompact = dumpFull
  dumpLine x =
    let
      caller = F.stubAddress' $ Tx.msgCaller x
      callee = F.stubAddress' $ Tx.msgCallee x
      val    = F.smart        $ Tx.msgValue x
      gas    = F.smart        $ Tx.msgGas x
      mdata  = toHex          $ Tx.msgData x
      mdata' = if length mdata == 0 then "null" else mdata
    in
      "<MsgCall: (" ++ caller ++ " => " ++ callee ++ ") " ++
      "V=" ++ val ++ " G=" ++ gas ++ " D=" ++ mdata' ++ ">"

instance Dump Account.AccountDB where
  dumpFull x = "AccountDB"
  dumpShallow x = "AccountDB"
  dumpCompact x = "AccountDB"
  dumpLine x = "AccountDB"

instance Dump Account.CodeDB where
  dumpFull x = "CodeDB"
  dumpShallow x = "CodeDB"
  dumpCompact x = "CodeDB"
  dumpLine x = "CodeDB"

instance Dump Value where
  dumpFull    = F.smart 
  dumpShallow = F.smart
  dumpCompact = F.smart
  dumpLine    = F.smart

instance Dump I.IntConfig where
  dumpFull       = unlines . map (\(k,v) -> k ++ ": " ++ v) . intConfFields
  dumpShallow    = unlines . map (\(k,v) -> k ++ ": " ++ v) . intConfFields
  dumpCompact    = unlines . map (\(k,v) -> k ++ ": " ++ v) . intConfFields
  dumpLine     c = 
    let abbr (k,v) = k ++ "=" ++ take 1 v
        kvs        = map abbr $ intConfFields c
    in "{" ++ intercalate ", " kvs ++ "}"

intConfFields :: I.IntConfig -> [(String, String)]
intConfFields x = map f I.types
  where f t = (tail $ show t, show $ I.getAction t x)


