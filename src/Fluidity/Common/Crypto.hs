module Fluidity.Common.Crypto where

import Prelude hiding (fail)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16

import Crypto.Hash (Digest, Keccak_256, hash)


keccak256 :: B8.ByteString -> B8.ByteString
keccak256 x = BA.convert (hash x :: Digest Keccak_256)
 
