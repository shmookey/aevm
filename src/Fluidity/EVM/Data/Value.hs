{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Value where

import Prelude hiding (Word)
import Control.DeepSeq
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as B8

import Fluidity.Common.Binary
import Fluidity.EVM.Data.Prov


type Word  = Integer

data Value = Value Word Prov
  deriving (Eq, Generic, NFData)

instance Ord Value where
  a <  b  = uint a <  uint b
  a >  b  = uint a >  uint b
  a <= b  = uint a <= uint b
  a >= b  = uint a >= uint b

instance Show Value where
  show = toHex

instance Bytes Value where
  toBytes (Value x _) = padBytes 32 $ toBytes x
  fromBytes = error "illegal conversion to value, use an originator instead"

instance Provenance Value where
  prov (Value _ x) = x


-- | Create a new value
value :: Integer -> Prov -> Value
value i = Value (mod256 i)

-- | Create a new value from bytes
bytevalue :: Bytes a => a -> Prov -> Value
bytevalue x prov = Value (mod256 i) prov
  where i = roll $ toBytes x

-- | Creates a mutated copy of a value with the same provenance
cloneWith :: (Integer -> Integer) -> Value -> Value
cloneWith f (Value x p) = Value (f x) p

-- | Represents the null value for uninitialised data like the balance of a nonexistent account
uninitialised :: Value
uninitialised = Value 0 Nul


-- Conversions
-- ---------------------------------------------------------------------

-- | The value interpreted as an unsigned integer
uint :: Value -> Integer
uint (Value x _) = x

-- | The value interpreted as a signed, twos-complement integer of 32-byte word size
sint :: Value -> Integer
sint (Value x _) = fromTwosComplement 32 x 

-- | The raw unsigned value as an Int
int :: Value -> Int
int = fromInteger . uint

-- | The shortest byte string that can represent the word, as opposed to `toBytes` which is always 32 bytes
bytes :: Value -> ByteString
bytes (Value x _) = unroll x

-- | The bytes value reduced to a simple 'not equals zero'
bool :: Value -> Bool
bool (Value x _) = x /= 0

-- | The bytes value padded to a 20-byte address
asAddress :: Value -> ByteString
asAddress = B8.drop 12 . toBytes

-- | Make an address value from a raw ByteString
mkAddress :: ByteString -> Value
mkAddress addr = value (roll addr) $ Env Address addr


-- Utility functions
-- ---------------------------------------------------------------------

mod256 :: Integer -> Word
mod256 = flip mod uint_max

uint_max :: Integer
uint_max = 2 ^ 256

