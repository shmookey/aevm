module Fluidity.EVM.Data.Operations where

import Prelude hiding (LT, GT)
import qualified Prelude as P
import Data.ByteString as B
import Data.Bits.ByteString ()
import Data.Bits as I (complement, xor, (.&.), (.|.))

import Fluidity.Common.Crypto (keccak256)
import Fluidity.Common.Binary (roll, toBytes)
import Fluidity.EVM.Data.ByteField as BF
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.Prov


-- Arithmetic, logical, comparison, bitwise and memory operations
-- ---------------------------------------------------------------------

add    a b = binop Add a b $ uint a + uint b
mul    a b = binop Mul a b $ uint a * uint b
sub    a b = binop Sub a b $ uint a - uint b
exp    a b = binop Exp a b $ uint a ^ uint b
mod    a b = binop Mod a b $ case (uint a, uint b) of (_, 0) -> 0
                                                      (x, y) -> x `P.mod` y
div    a b = binop Div a b $ case (uint a, uint b) of (_, 0) -> 0
                                                      (x, y) -> x `P.div` y
gt     a b = binop GT   a b $ if uint a >  uint b then 1 else 0
lt     a b = binop LT   a b $ if uint a <  uint b then 1 else 0
eq     a b = binop Eq   a b $ if uint a == uint b then 1 else 0
sgt    a b = binop SGT  a b $ if sint a >  sint b then 1 else 0 
slt    a b = binop SLT  a b $ if sint a <  sint b then 1 else 0 
and    a b = binop And  a b . roll $ toBytes a   .&.   toBytes b
or     a b = binop Or   a b . roll $ toBytes a   .|.   toBytes b
xor    a b = binop Xor  a b . roll $ toBytes a `I.xor` toBytes b
byte   a b = binop Byte a b . toInteger $ if int a < 32 then B.index (toBytes b) (int a) else 0
iszero   a = unaop IsZero a $ if bool a then 0 else 1
bitnot   a = unaop Not a . roll . complement $ toBytes a
size     m = unaop Size m . toInteger $ BF.size m
sha3 a b m = memop SHA3 a b m . roll . keccak256 . toBytes $ fullslice (int a) (int b) m


-- Wrapping functions
-- ---------------------------------------------------------------------

unaop :: Provenance a => UnaOp -> a -> Integer -> Value
unaop o a x = value x $ UnaOp o (toBytes x) (prov a)

binop :: BinOp -> Value -> Value -> Integer -> Value
binop o a b x = value x $ BinOp o (toBytes x) (prov a) (prov b)

memop :: MemOp -> Value -> Value -> ByteField -> Integer -> Value
memop o a b m x = value x $ MemOp o (toBytes x) (prov a) (prov b) (prov m)

