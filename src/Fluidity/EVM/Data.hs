{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data where

import Prelude hiding (GT, LT, Word, and, or)
import Data.Bits.ByteString
import Data.Char (chr, ord)
import Data.ByteString.Char8 (ByteString)
import Data.Bits ((.&.), (.|.))
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8


import Text.Structured (Structured(fmt))
import qualified Fluidity.Common.Crypto as Crypto
import qualified Fluidity.Common.Binary as Bin


type Byte = Char

data Value
  = Word Integer Prov
  | Byte Byte Prov
  deriving (Eq, Show, Generic, NFData)


data Prov
  = Add    Value Value
  | Mul    Value Value
  | Sub    Value Value
  | Div    Value Value
  | SDiv   Value Value
  | Mod    Value Value
  | Exp    Value Value
  | IsZero Value
  | GT     Value Value
  | LT     Value Value
  | SGT    Value Value
  | SLT    Value Value
  | And    Value Value
  | Or     Value Value
  | Xor    Value Value
  | Not    Value
  | ByteOf Value Value
  | SHA3   Value [Value]                -- SHA3 mptr bytes
  | Push Int Integer                    -- Push length
  | MLoad Value [Value]                 -- MLoad offset bytes
  | MStore Value Value                  -- MStore offset byte
  | MStore8 Value Value                 -- MStore8 offset byte
  | MSize
  | CallDataSize
  | CallDataCopy Value Value Value Prov -- CallDataCopy dptr sptr sz prov
  | CallDataLoad Value Prov             -- CallDataLoad offset word
  | CodeCopy Value Value Value Prov     -- CodeCopy dptr sptr sz prov
  | CallCopy Value Value Prov           -- CallCopy mptr sz prov
  | LogCopy Value Value Prov            -- CallCopy mptr sz prov
  | SStore Value Value                  -- SStore key val
  | SLoad Value Value                   -- SLoad key val
  | RetVal
  | CallData
  | Storage
  | Code
  | Balance
  | Caller
  | Block
  | BlockNumber
  | BlockHash
  | BlockTime
  | Nil
  | CallValue
  | DebugInput
  | IllegalConversion
  | Intermediate
  | Internal
  | Blockchain
  | GasPrice
  | Difficulty
  | GasMeter
  | InitialGas
  | Snapshot
  | Address Prov
  deriving (Eq, Show, Generic, NFData)

provenance :: Value -> Prov
provenance v = case v of 
  Word _ x -> x
  Byte _ x -> x

-- Origination constructions
-- ---------------------------------------------------------------------



mkPush n v     = let v' = mod256 v in Word v' $ Push n v'
mkBlockNumber  = flip Word BlockNumber  . mod256
mkBlockHash    = flip Word BlockHash    . mod256
mkBlockTime    = flip Word BlockTime    . mod256
mkRetVal       = flip Word RetVal       . mod256
mkDebugInput   = flip Word DebugInput   . mod256
mkCallValue    = flip Word CallValue    . mod256
mkBalance      = flip Word Balance      . mod256
mkCaller       = flip Word Caller       . mod256
mkIntermediate = flip Word Intermediate . mod256
mkInternal     = flip Word Internal     . mod256
mkGasMeter     = flip Word GasMeter     . mod256
mkInitialGas   = flip Word InitialGas   . mod256
mkMSize        = flip Word MSize        . mod256
mkGasPrice     = flip Word GasPrice     . mod256
mDifficulty    = flip Word Difficulty   . mod256
mkSStore k v   = Word (mod256 $ asUInt v) (SStore k v)
mkSLoad k v    = Word (mod256 $ asUInt v) (SLoad k v)
mkNil          = Word 0 Nil

mkAddress p    = flip Word (Address p)  . mod256

origBlockchain = flip Word Blockchain   . mod256

-- Arithmetic and logical operations
-- ---------------------------------------------------------------------

-- When treated as a number, a Value behaves as an unsigned 256-bit integer
--instance Num Value where
--  a + b = Word (mod256 $ asUInt a + asUInt b) (Add a b)
--  a * b = Word (mod256 $ asUInt a * asUInt b) (Mul a b)
--  a - b = Word (mod256 $ asUInt a - asUInt b) (Sub a b)
--  abs a = a
--  fromInteger a = Word a IllegalConversion

instance Ord Value where
  a < b     = (asUInt a)  <    (asUInt b)
  a > b     = (asUInt a)  >    (asUInt b)
  a <= b    = (asUInt a)  <=   (asUInt b)
  a >= b    = (asUInt a)  >=   (asUInt b)

add a b = Word (mod256 $ asUInt a + asUInt b) (Add a b)
mul a b = Word (mod256 $ asUInt a * asUInt b) (Mul a b)
sub a b = Word (mod256 $ asUInt a - asUInt b) (Sub a b)

umod :: Value -> Value -> Value
umod a b = case (asUInt a, asUInt b) of
  (_ , 0)  -> Word 0 (Mod a b)
  (a', b') -> Word (mod256 $ a' `mod` b') (Mod a b)

udiv :: Value -> Value -> Value
udiv a b = case (asUInt a, asUInt b) of
  (_ , 0)  -> Word 0 (Div a b)
  (a', b') -> Word (mod256 $ asUInt a `div` asUInt b) (Div a b)

pow :: Value -> Value -> Value
pow a b = Word (mod256 $ asUInt a ^ asUInt b) (Exp a b)

--sdiv :: Value -> Value -> Value
--sdiv a b = case (asSInt a, asSInt b) of
--  (_ , 0) -> Word 0 (SDiv a b)
--  (a',b') -> Word (mod256 $ a' / b') (SDiv a b)


-- Convenience operators
-- ---------------------------------------------------------------------

(/+/) :: Value -> Value -> Value
(/+/) = add

(/*/) :: Value -> Value -> Value
(/*/) = mul

(/-/) :: Value -> Value -> Value
(/-/) = sub

(/**/) :: Value -> Value -> Value
(/**/) = pow

(///) :: Value -> Value -> Value
(///) = udiv

(/>/) :: Value -> Value -> Value
(/>/) = gt

(/</) :: Value -> Value -> Value
(/</) = lt

(/.>/) :: Value -> Value -> Value
(/.>/) = sgt

(/.</) :: Value -> Value -> Value
(/.</) = slt

(/==/) :: Value -> Value -> Value
(/==/) = eq

(/&/) :: Value -> Value -> Value
(/&/) = and

(/|/) :: Value -> Value -> Value
(/|/) = or

(/^/) :: Value -> Value -> Value
(/^/) = xor

(/~/) :: Value -> Value
(/~/) = bitwisenot

(/!/) :: Value -> Value
(/!/) = iszero

(/!!/) :: Value -> Value -> Value
(/!!/) = byteof



-- Comparisons and bitwise logic operations
-- ---------------------------------------------------------------------

gt :: Value -> Value -> Value
gt a b = Word (if a' > b' then 0 else 1) (GT a b)
  where a' = asUInt a
        b' = asUInt b

lt :: Value -> Value -> Value
lt a b = Word (if a' < b' then 0 else 1) (LT a b)
  where a' = asUInt a
        b' = asUInt b

eq :: Value -> Value -> Value
eq a b = Word (if a' == b' then 0 else 1) (LT a b)
  where a' = asUInt a
        b' = asUInt b

sgt :: Value -> Value -> Value
sgt a b = Word (if a' > b' then 0 else 1) (SGT a b)
  where a' = asSInt a
        b' = asSInt b

slt :: Value -> Value -> Value
slt a b = Word (if a' < b' then 0 else 1) (SLT a b)
  where a' = asSInt a
        b' = asSInt b

iszero :: Value -> Value
iszero v = Word (if asBool v then 1 else 0) (IsZero v)

and :: Value -> Value -> Value
and a b = Word (Bin.roll $ a' .&. b') (And a b)
  where a' = asBytesWord a
        b' = asBytesWord b

or :: Value -> Value -> Value
or a b = Word (Bin.roll $ a' .|. b') (Or a b)
  where a' = asBytesWord a
        b' = asBytesWord b

xor :: Value -> Value -> Value
xor a b = Word (Bin.roll $ a' `Bits.xor` b') (Xor a b)
  where a' = asBytesWord a
        b' = asBytesWord b

bitwisenot :: Value -> Value
bitwisenot v = Word (Bin.roll $ Bits.complement v') (Not v)
  where v' = asBytesWord v

byteof :: Value -> Value -> Value
byteof k v =
  let
    (i, bs) = (toInt k, asBytesWord v)
    b       = if i > 31 
              then chr 0
              else B8.head $ B8.drop i bs
  in
    Byte b (ByteOf k v)


-- Intrinsic representations
-- ---------------------------------------------------------------------

--fromSInt :: Integer -> Value
--fromSInt x =
--  let word = Bin.padBytes 32 $ Bin.unroll (abs x)
--  in Bits.complement 

asUInt :: Value -> Integer
asUInt v = case v of
  Word x _ -> x
  Byte x _ -> toInteger (ord x)

asSInt :: Value -> Integer
asSInt v = 
  let word = asBytesWord v
  in if Bits.testBit word 0
     then negate (Bin.roll (Bits.complement word) + 1)
     else asUInt v

-- | The bytes value padded to a 32-byte word
asBytesWord :: Value -> ByteString
asBytesWord = Bin.padBytes 32 . asBytesMin

-- | The byte value without leading zeroes
asBytesMin :: Value -> ByteString
asBytesMin v = case v of
  Word x _ -> Bin.unroll x
  Byte x _ -> B8.singleton x

-- | The bytes value padded to a 20-byte address
asBytesAddress :: Value -> ByteString
asBytesAddress = B8.take 20 . asBytesWord

asBool :: Value -> Bool
asBool v = (asUInt v) == 0


-- Text formatting
-- ---------------------------------------------------------------------

instance Structured Value where
  fmt v = case v of
    Word x _ -> fmt $ formatWord v
    Byte x _ -> fmt $ formatHex v

instance Structured ByteField where
  fmt bf = fmt (Bin.toHex $ fieldToByteString bf :: String)


-- | Format a value as a full 32 byte word in hexadecimal
formatWord :: Value -> String
formatWord = B8.unpack . B16.encode . asBytesWord

-- | Format a value in hexadecimal with no padding
formatHex :: Value -> String
formatHex = B8.unpack . B16.encode . asBytesMin

-- | Format a hex stub from the lower 4 bytes of a value
formatStubL :: Value -> String
formatStubL = drop 56 . formatWord

-- | Format a hex stub from the upper 4 bytes of a value
formatStub :: Value -> String
formatStub = take 8 . formatWord

-- | Format a value as an address, starting with 0x
formatAddress :: Value -> String
formatAddress = ("0x" ++) . B8.unpack . B16.encode . asBytesAddress

-- | Format a value as an unsigned integer
formatUInt :: Value -> String
formatUInt = show . asUInt


-- ByteField container
-- ---------------------------------------------------------------------

-- Purpose: to addresses the mismatch between word and byte addressing.
-- Use qualified names for basic list operations to avoid potential overlaps.

data ByteField = ByteField [Value]
  deriving (Eq, Show, Generic, NFData)

instance Monoid ByteField where
  mempty = fresh
  mappend (ByteField a) (ByteField b) = ByteField (a ++ b)
  mconcat = foldl mappend fresh 

fresh :: ByteField
fresh = ByteField []

size :: ByteField -> Int
size (ByteField xs) = length xs

prefill :: Prov -> ByteString -> ByteField
prefill prov = ByteField . List.map (flip Byte prov) . B8.unpack

nil :: Value
nil = Byte (chr 0) Nil

sha3 :: ByteField -> Value -> Value -> Value
sha3 src ptr sz =
  let
    bf@(ByteField range) = copy id src fresh (mkIntermediate 0) ptr sz
    hash = Bin.roll . Crypto.keccak256 $ fieldToByteString bf
  in
    Word hash (SHA3 ptr range)

calldatasize :: ByteField -> Value
calldatasize (ByteField bf) = Word (toInteger $ List.length bf) CallDataSize

-- | Read a word from the bytefield, progressing its provenance as if it were calldata
calldataload :: ByteField -> Value -> Value
calldataload field offset =
  let
    ptr            = toInt offset
    (ByteField xs) = allocateTo (ptr+32) field
    range          = fst . List.splitAt 32 $ List.drop ptr xs 
    value          = Bin.roll . B8.pack $ List.map (getByte . toByte) range
  in
    Word value (CallDataLoad offset CallData)

calldatacopy :: ByteField -> ByteField -> Value -> Value -> Value -> ByteField
calldatacopy src dst dptr sptr sz =
  copy (CallDataCopy dptr sptr sz) src dst dptr sptr sz

codecopy :: ByteField -> ByteField -> Value -> Value -> Value -> ByteField
codecopy src dst dptr sptr sz =
  copy (CodeCopy dptr sptr sz) src dst dptr sptr sz

callcopy :: ByteField -> Value -> Value -> ByteField
callcopy src sptr sz =
  copy (CallCopy sptr sz) src fresh (mkIntermediate 0) sptr sz

logcopy :: ByteField -> Value -> Value -> ByteField
logcopy src sptr sz =
  copy (LogCopy sptr sz) src fresh (mkIntermediate 0) sptr sz

copy :: (Prov -> Prov) -> ByteField -> ByteField -> Value -> Value -> Value -> ByteField
copy f srcField dstField dstOffset srcOffset size =
  let
    (sptr, dptr, sz) = (toInt srcOffset, toInt dstOffset, toInt size)
    (ByteField src)  = allocateTo (sptr + sz) srcField
    (ByteField dst)  = allocateTo (sptr + sz) dstField
    srcRange         = List.take sz $ List.drop sptr src
    (dstLo, dstMid)  = List.splitAt dptr dst
    dstHi            = List.drop sz dstMid
  in
    ByteField $ List.concat [dstLo, List.map (provMap f) srcRange, dstHi]

-- | Read a word from the bytefield, progressing its provenance as if it were memory
mload :: ByteField -> Value -> Value
mload field offset =
  let
    ptr            = toInt offset
    (ByteField xs) = allocateTo (ptr+32) field
    range          = fst . List.splitAt 32 $ List.drop ptr xs 
    value          = Bin.roll . B8.pack $ map (getByte . toByte) range
  in
    Word value (MLoad offset range)

-- | Write a whole word value to the bytefield, progressing its provenance as if it were memory
mstore :: ByteField -> Value -> Value -> ByteField
mstore field offset value =
  let
    index          = toInt offset
    (ByteField xs) = allocateTo (index + 32) field 
    (lo, mid)      = List.splitAt index xs
    hi             = snd $ List.splitAt 32 mid
    prov           = MStore offset value
    bytes          = List.map (flip Byte prov) . B8.unpack $ asBytesWord value
  in
    ByteField $ concat [lo, bytes, hi]

-- | Write the low byte of a word to an address in memory, progressing its provenance as if it were memory
mstore8 :: ByteField -> Value -> Value -> ByteField
mstore8 field offset value =
  let
    (index, bytes) = (toInt offset, asBytesWord value)
    (ByteField xs) = allocateTo index field 
    (lo, _ : hi)   = List.splitAt index xs
    value'         = Byte (B8.last bytes) (MStore8 offset value)
  in
    ByteField $ concat [lo, return value', hi]
    
-- always allocates more than needed, since allocations tend to be in chunks
allocateTo :: Int -> ByteField -> ByteField
allocateTo i (ByteField xs) =
  let
    d = i - length xs
  in
    if i >= 0
    then ByteField $ xs ++ (take (d+32) $ repeat nil)
    else ByteField xs


-- Internal utility functions
-- ---------------------------------------------------------------------

-- | Unsafe force a word to a single byte, error if the value is larger
toByte :: Value -> Value
toByte v = case v of
  Byte _ _ -> v
  Word x h -> let bytes = Bin.padBytes 1 $ Bin.unroll x
              in if B8.length bytes > 1
                 then error "internal error: oversized byte"
                 else Byte (B8.head bytes) h

-- | Unsafe extract a byte value, error if the value is not Byte
getByte :: Value -> Byte
getByte v = case v of
  Byte x _ -> x
  _        -> error "internal error: non-byte value in byte-addressed space"

toInt :: Value -> Int
toInt = fromInteger . asUInt

mod256 :: Integer -> Integer
mod256 = flip mod (2^256)

provMap :: (Prov -> Prov) -> Value -> Value
provMap f v = case v of
  Word x p -> Word x (f p)
  Byte x p -> Byte x (f p)

fieldToByteString :: ByteField -> ByteString
fieldToByteString (ByteField bs) = B8.pack $ map getByte bs


