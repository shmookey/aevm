{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Fluidity.EVM.Text where

import Prelude hiding (Value)
import Data.Text (Text)
import Data.Char (toUpper)
import Data.ByteString (ByteString)
import Text.Printf (printf)
import qualified Data.Map as Map

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16

import Text.Structured

import Fluidity.Common.Binary (padBytes, toHex) 
import Fluidity.EVM.Data (Value, ByteField
  , asBytesWord, asBytesMin
  , formatAddress, formatUInt, formatStub
  , fieldToByteString )
import Fluidity.EVM.Types
import Fluidity.EVM.VM (VM)
import Fluidity.EVM.Blockchain (MessageCall(..))
import qualified Fluidity.EVM.Data as Data
import qualified Fluidity.EVM.VM as VM


instance Structured MessageCall where
  fmt (MessageCall caller callee value gas bytes) = 
    "CALL" ~- "[" ~~ formatStub caller ~~ "]" 
   ~- "=>" ~- "[" ~~ formatStub callee ~~ "]"
   ~- formatUInt value ~- formatUInt gas ~- fieldToByteString bytes

formatStorage :: Storage -> String
formatStorage = Prelude.unlines . map f . Map.toList 
  where f (k,v) = formatStub k ++ " " 
               ++ toHex (asBytesWord v) ++ " " 
               ++ formatUInt v

formatExternalCall :: ExtCall -> String
formatExternalCall (gas, addr, balance, cdata, mptr, retsz) =
  T.unpack . typeset $ "CALL" 
    ~- formatStub addr ~- formatUInt balance ~- formatUInt gas ~- fieldToByteString cdata ~- formatUInt retsz

instance Structured Op where
  fmt op = case op of
    Label x     -> x ~~ ":"
    Comment x   -> "--" ~- x
    PushLabel x -> "PUSH [" ~~ x ~~ "]"
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
    CData x     -> Empty
    x           -> fmt . map toUpper $ show x

instance Structured VM.Error where
  fmt e = case e of
    VM.StackUnderflow      -> fmt "Stack underflow."
    VM.InvalidJump x       -> "Invalid jump target: 0x" ~~ x
    VM.InvalidPC x         -> "Invalid program counter value:" ~- x
    VM.NotImplemented x    -> "Not implemented:" ~- x
    _                      -> "Error: " ~- show e

instance Structured Integer where
  fmt x = if x >= 0 
          then "0x" ~~ (T.pack $ printf "%064x" x)
          else "-0x" ~~ (T.pack $ printf "%064x" (abs x))

instance Structured Program where
  fmt (Program xs) = block . map inst $ zip [0..] xs
    where inst :: (Int, Op) -> Fragment
          inst (i, Label x) = (hexdigits 4 i) ~- x ~~ ":"
          inst (_, CData _) = Empty
          inst (i, x)       = (hexdigits 4 i) ~- "  " ~~ x 

instance Structured SourceMap where
  fmt (SourceMap (Program xs) ys) = block . map inst $ zip ys xs
    where inst :: (Int, Op) -> Fragment
          inst (i, Label x) = (hexdigits 4 i) ~- x ~~ ":"
          inst (_, CData _) = Empty
          inst (i, x)       = (hexdigits 4 i) ~- "  " ~~ x

instance Structured ByteString where
  fmt = fmt . (++) "0x" . B8.unpack . B16.encode . padBytes 1

instance Structured Log where
  fmt (Log mem topics) = mem ~- phrase topics

instance Show (VM ()) where
  show x = "<internal vm state>"



