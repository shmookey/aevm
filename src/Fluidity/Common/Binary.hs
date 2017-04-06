{-# LANGUAGE FlexibleInstances #-}

module Fluidity.Common.Binary where

import Data.Bits (Bits, (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32, Word64)
import Text.Printf (printf)
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T


{- Types and functions for working with binary data -}

class Bytes a where
  toBytes :: a -> B.ByteString
  fromBytes :: B.ByteString -> a

instance Bytes B.ByteString where
  toBytes = id
  fromBytes = id

instance Bytes BL.ByteString where
  toBytes = BL.toStrict
  fromBytes = BL.fromStrict

instance Bytes [Char] where
  toBytes = B8.pack
  fromBytes = B8.unpack

instance Bytes Text where
  toBytes = toBytes . T.unpack
  fromBytes = T.pack . fromBytes

instance Bytes Integer where
  toBytes = unroll
  fromBytes = roll

instance Bytes Int where
  toBytes = unroll
  fromBytes = fromInteger . roll

-- Hex data
-- ---------------------------------------------------------------------

toHex :: (Bytes a, Bytes b) => a -> b
toHex = fromBytes . B16.encode . toBytes

fromHex :: (Bytes a, Bytes b) => a -> b
fromHex = fromBytes . fst . B16.decode . toBytes

toHexWord :: (Bytes a, Bytes b) => Int -> a -> b
toHexWord sz = toHex . padBytes sz . toBytes

fromHexOnly :: (Bytes a, Bytes b) => a -> Maybe b
fromHexOnly x = 
  let (bs, invalids) = B16.decode (toBytes x)
  in if B.null invalids then Just (fromBytes bs) else Nothing


-- Manipulation
-- ---------------------------------------------------------------------

padBytes :: Int -> ByteString -> ByteString
padBytes n bs =
  if B.length bs >= n then bs
  else 
    let len = B.length bs
        z   = B.pack $ take (n - len) (repeat 0)
    in z `B.append` bs

toBool :: Word -> Bool
toBool x = not $ x == 0


-- Bytestring <-> Integer
-- ---------------------------------------------------------------------

roll :: ByteString -> Integer
roll = B.foldl' unstep 0
  where unstep a b = a `shiftL` 8 .|. fromIntegral b

unroll :: (Integral a, Bits a) => a -> ByteString
unroll = B.reverse . B.unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

rollInt :: ByteString -> Int
rollInt = fromInteger . roll


-- Formatting and display
-- ---------------------------------------------------------------------

hex8 :: Word8 -> String
hex8 x = printf "0x%02x" x

hex16 :: Word16 -> String
hex16 x = printf "0x%04x" x

hex32 :: Word16 -> String
hex32 x = printf "0x%08x" x

hex64 :: Word64 -> String
hex64 x = printf "0x%16x" x


