module Fluidity.EVM.Text.Disassembly where

import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import System.Console.ANSI
import qualified Data.ByteString as B

import Control.Monad.Result
import Text.Structured (toString)

import Fluidity.Common.Binary
import Fluidity.Common.ANSI
import Fluidity.EVM.Data.ByteField (fromByteString)
import Fluidity.EVM.Data.Prov (Prov(Nul))
import Fluidity.EVM.Data.Value (Value, bytes)
import Fluidity.EVM.Data.Format (fmtCodePtr)
import Fluidity.EVM.Data.Bytecode (Op(..), disassembleWithPtr)
import Fluidity.EVM.Text.Util

disassembly :: ByteString -> String
disassembly = disassemblyFromOffset 0

disassemblyFromOffset :: Int -> ByteString -> String
disassemblyFromOffset q =
  unlines . map (\(i, x) -> toString (fmtCodePtr $ i+q) ++ " " ++ showOp x) 
          . disassembleWithPtr 
          . fromByteString Nul

surroundingCode :: Int -> Int -> Int -> ByteString -> String
surroundingCode pre post p bs =
  let
    (l, m, r) = splitDisassembly p bs
    (ls, rs)  = on (,) (lines . indent 4) l r
    l'        = drop (length ls - pre) ls
    r'        = take post rs
    m'        = "*** " ++ m
  in
    unlines $ l' ++ [m'] ++ r'

splitDisassembly :: Int -> ByteString -> (String, String, String)
splitDisassembly p bs =
  let
    (l, r)  = B.splitAt p bs
    (l',r') = (disassembly l, disassemblyFromOffset p r)
    (m, rs) = fromMaybe ("error",["disasm error"]) $ uncons (lines r')
    r''     = unlines rs
  in
    (l', m, r'')

showOp :: Op -> String
showOp op = 
  let
    constant :: Value -> String
    constant = toString . colour Blue . toHexS . bytes

    opcode :: String -> String
    opcode   = toString . colour Yellow
  in case op of
    Push1  x -> opcode "PUSH1 "  ++ constant x
    Push2  x -> opcode "PUSH2 "  ++ constant x
    Push3  x -> opcode "PUSH3 "  ++ constant x
    Push4  x -> opcode "PUSH4 "  ++ constant x
    Push5  x -> opcode "PUSH5 "  ++ constant x
    Push6  x -> opcode "PUSH6 "  ++ constant x
    Push7  x -> opcode "PUSH7 "  ++ constant x
    Push8  x -> opcode "PUSH8 "  ++ constant x
    Push9  x -> opcode "PUSH9 "  ++ constant x
    Push10 x -> opcode "PUSH10 " ++ constant x
    Push11 x -> opcode "PUSH11 " ++ constant x
    Push12 x -> opcode "PUSH12 " ++ constant x
    Push13 x -> opcode "PUSH13 " ++ constant x
    Push14 x -> opcode "PUSH14 " ++ constant x
    Push15 x -> opcode "PUSH15 " ++ constant x
    Push16 x -> opcode "PUSH16 " ++ constant x
    Push17 x -> opcode "PUSH17 " ++ constant x
    Push18 x -> opcode "PUSH18 " ++ constant x
    Push19 x -> opcode "PUSH19 " ++ constant x
    Push20 x -> opcode "PUSH20 " ++ constant x
    Push21 x -> opcode "PUSH21 " ++ constant x
    Push22 x -> opcode "PUSH22 " ++ constant x
    Push23 x -> opcode "PUSH23 " ++ constant x
    Push24 x -> opcode "PUSH24 " ++ constant x
    Push25 x -> opcode "PUSH25 " ++ constant x
    Push26 x -> opcode "PUSH26 " ++ constant x
    Push27 x -> opcode "PUSH27 " ++ constant x
    Push28 x -> opcode "PUSH28 " ++ constant x
    Push29 x -> opcode "PUSH29 " ++ constant x
    Push30 x -> opcode "PUSH30 " ++ constant x
    Push31 x -> opcode "PUSH31 " ++ constant x
    Push32 x -> opcode "PUSH32 " ++ constant x
    _        -> opcode . uppercase $ show op

    
