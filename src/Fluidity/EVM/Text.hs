{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Fluidity.EVM.Text where

import Prelude hiding (Value)
import Data.Text (Text)
import Data.Char (toUpper)
import Data.ByteString (ByteString)
import Text.Printf (printf)
import System.Console.ANSI
import qualified Data.Map as Map

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16

import Text.Structured
import qualified Text.Structured as TS

import Fluidity.Common.ANSI
import Fluidity.Common.Binary
import Fluidity.EVM.Data.ByteField
import Fluidity.EVM.Data.Format
import Fluidity.EVM.Data.Transaction
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Types
import Fluidity.EVM.Core.VM (VM)
import Fluidity.EVM.Core.Interrupt (Interrupt, IntType)
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Bytecode
import Fluidity.EVM.Core.Blockchain
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.Interrupt as INT

instance Structured MessageCall where
  fmt (MessageCall caller callee value gas bytes) = 
   Call ~- "[" ~~ stubAddress caller ~~ "]" 
   ~- "=>" ~- "[" ~~ stubAddress callee ~~ "]"
   ~- currency value ~- uint gas ~- toHexS bytes

instance Structured Interrupt where
  fmt int = case int of 
    INT.JumpI c p -> JumpI ~- boolean c ~- toHexShort p
    INT.Jump p    -> Jump ~- toHexShort p
    INT.Emit bf v -> let op = case length v of { 0 -> Log0 ; 1 -> Log1 ; 2 -> Log2 ; 3 -> Log3 ; 4 -> Log4 }
                     in op ~- abbreviated 12 (toBytes bf) ~-  phrase (map smart v)
    INT.Stop      -> fmt Stop
    _             -> fmt $ show int

formatInterrupt :: Interrupt -> String
formatInterrupt = formatIntType . INT.intType

formatIntType :: IntType -> String
formatIntType = toString . highlight Red . tail . show

formatStorage :: StorageDB -> String
formatStorage = Prelude.unlines . map f . storageEntriesDB
  where f (k,v) = stub k ++ " " 
               ++ toHexS v ++ " " 
               ++ show (uint v)

formatExternalCall gas addr balance cdata mptr retsz = T.unpack .
  typeset $ Call ~- stubAddress addr ~- currency balance ~- uint gas ~- toHexS cdata ~- uint retsz

instance Structured Op where
  fmt op = colour Yellow $ case op of
    Push1  x    -> "PUSH1"  ~- x
    Push2  x    -> "PUSH2"  ~- x
    Push3  x    -> "PUSH3"  ~- x
    Push4  x    -> "PUSH4"  ~- x
    Push5  x    -> "PUSH5"  ~- x
    Push6  x    -> "PUSH6"  ~- x
    Push7  x    -> "PUSH7"  ~- x
    Push8  x    -> "PUSH8"  ~- x
    Push9  x    -> "PUSH9"  ~- x
    Push10 x    -> "PUSH10" ~- x
    Push11 x    -> "PUSH11" ~- x
    Push12 x    -> "PUSH12" ~- x
    Push13 x    -> "PUSH13" ~- x
    Push14 x    -> "PUSH14" ~- x
    Push15 x    -> "PUSH15" ~- x
    Push16 x    -> "PUSH16" ~- x
    Push17 x    -> "PUSH17" ~- x
    Push18 x    -> "PUSH18" ~- x
    Push19 x    -> "PUSH19" ~- x
    Push20 x    -> "PUSH20" ~- x
    Push21 x    -> "PUSH21" ~- x
    Push22 x    -> "PUSH22" ~- x
    Push23 x    -> "PUSH23" ~- x
    Push24 x    -> "PUSH24" ~- x
    Push25 x    -> "PUSH25" ~- x
    Push26 x    -> "PUSH26" ~- x
    Push27 x    -> "PUSH27" ~- x
    Push28 x    -> "PUSH28" ~- x
    Push29 x    -> "PUSH29" ~- x
    Push30 x    -> "PUSH30" ~- x
    Push31 x    -> "PUSH31" ~- x
    Push32 x    -> "PUSH32" ~- x
    x           -> fmt . TS.padRight 6 . map toUpper $ show x

instance Structured VM.Error where
  fmt e = case e of
    VM.StackUnderflow      -> fmt "Stack underflow."
    VM.InvalidJump x       -> "Invalid jump target: 0x" ~~ x
    VM.InvalidPC x         -> "Invalid program counter value:" ~- x
    VM.NotImplemented x    -> "Not implemented:" ~- x
    _                      -> "Error: " ~- show e

instance Structured Integer where
  fmt = fmt . show

instance Structured Program where
  fmt (Program xs) = block . map inst $ zip [0..] xs
    where inst :: (Int, Op) -> Fragment
          inst (i, x)       = (hexdigits 4 i) ~- "  " ~~ x 

instance Structured ByteString where
  fmt = fmt . (++) "0x" . B8.unpack . B16.encode . padBytes 1

instance Structured Log where
  fmt (Log mem topics) = mem ~- phrase topics

instance Show (VM ()) where
  show x = "<internal vm state>"



