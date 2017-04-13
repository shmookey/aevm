{- ByteField - History-preserving byte arrays

ByteFields provide per-byte provenance tracking of their contents, but they are
invisible to the history of the values they contain unless an original value is
transformed in some way by being read from the array. There are two ways that
this can happen:

  1. If a read operation is at an offset to a written region, it is marked as a
     "transposing read", which creates a provenance entry containing the region
     of bytes read by the operation, and the byte offset by which it has been
     shifted. 

     Example: If a 32-byte word is written starting at `ptr`, 32 bytes are read
              starting from `ptr + 1`, the result will be the original word bit
              shifted to the left by 8 bits, and the original Prov value `prov`
              will be wrapped as `TransRead (x << 8) -1 prov`.

  2. If a read operation spans across regions that were written separately, it
     is considered a "dirty read". In this case, the result will be a composite
     of data from original values appended together, and the Prov of each value
     will be tupled with a byte offset and wrapped in a DirtyRead.
    
     Example: If two 32-byte words are written at `ptr` and `ptr + 32` and read
              from `ptr + 4`, the resulting Prov entry will be:
              `DirtyRead (x1 << 32 && x2 >> 224) [(-4, p1), (28, p2)]`

Note that these rules only apply when the read operation produces a value, such
as in loading data from memory or storage onto the stack. Copying data directly
between ByteFields does not trigger this behaviour - the source offset of each
byte is copied along with the value, so if multiple offset copy operations have
the effect of placing the original bytes in their original order then the value
can still be read "cleanly", with no resulting entry in its provenance.

A ByteField can have a default provenance which is applied instead of Nul when
reading from uninitialised regions. When two ByteFields are concatenated, the 
default provenance of the left hand side ByteField is always the default used
for the resulting ByteField, even if it is undefined - in this case the new
ByteField also has an undefined default provenance. The default provenance also
has an associated offset, reflecting the starting byte position in the original
source. This offset value is increased when bytes are dropped from the head of
the field, as in most slicing and sublist operations,

The purpose of the default provenance is to ensure that user-controlled input
areas are recognised as such in dependency analysis, even if the user has not
provided any input in that region. It only applies when a read overextends the
input provided and padding is generated to the right. Left-padding operations
use the Nul provenance.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.ByteField where

import Prelude hiding (head, drop, null, take)
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Semigroup (Semigroup, (<>))
import qualified Data.List as L
import qualified Data.ByteString as B

import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.Prov
import Fluidity.Common.Binary


data ByteField = ByteField 
  { bfData    :: [ByteP]
  , bfDefault :: (Prov, Int)
  }
  deriving (Eq, Generic, NFData)

data ByteP = ByteP { bpVal :: Word8, bpProv :: Prov, bpOffset :: Int }
  deriving (Eq, Show, Generic, NFData)

instance Bytes ByteField where
  toBytes (ByteField bps _) = B.pack $ map bpVal bps
  fromBytes = error "illegal conversion to bytefield, use pack instead"

instance Show ByteField where
  show = toHex

instance Semigroup ByteField where
  a <> b = mappend a b

instance Monoid ByteField where
  mempty = ByteField [] (Nul, 0)
  mappend (ByteField a p) (ByteField b _) = ByteField (mappend a b) p

instance Provenance ByteField where
  prov = readProvenance


-- Basic operations
-- ---------------------------------------------------------------------

-- | An empty ByteField with a given default provenance and no offset
empty :: Prov -> ByteField
empty p = pack (p,0) []

-- | The size of the ByteField in bytes
size :: ByteField -> Int
size (ByteField bps _) = length bps

-- | Nul-pad a ByteField to the left
padLeft :: Int -> ByteField -> ByteField
padLeft sz bf = 
  if size bf >= sz
  then
    bf
  else
    let n = sz - size bf
    in (pack (bfDefault bf) . L.take n . repeat $ ByteP 0 Nul 0) <> bf

-- | Nul-pad a ByteField to the left
padRight :: Int -> ByteField -> ByteField
padRight sz bf = 
  if size bf >= sz
  then bf
  else 
    let 
      len     = size bf
      (pv, o) = bfDefault bf
      blanks  = map (\i -> ByteP 0 pv (o + len + i)) [0..]
    in
     bf <> (pack (bfDefault bf) $ L.take (sz - len) blanks) 


-- Creating, wrapping and unwrapping ByteFields
-- ---------------------------------------------------------------------

-- | Unwrap a ByteField to get a list of `ByteP`s
unpack :: ByteField -> [ByteP]
unpack (ByteField bps _) = bps

-- | Wrap a list of `ByteP`s into a ByteField using a given default provenance
pack :: (Prov, Int) -> [ByteP] -> ByteField
pack dp = flip ByteField dp

-- | Create a ByteField from a ByteString with a common provenance and no offset
fromByteString :: Prov -> ByteString -> ByteField
fromByteString p = flip ByteField (p, 0) . map mkByte . zip [0..] . B.unpack
  where mkByte (i, b) = ByteP b p i

-- | Like `fromByteString`, but with an offset
fromByteStringOffset :: Int -> Prov -> ByteString -> ByteField
fromByteStringOffset o p = flip ByteField (p, o) . map mkByte . zip [o..] . B.unpack
  where mkByte (i, b) = ByteP b p i

-- | Read a Value into a ByteField
fromValue :: Value -> ByteField
fromValue x = fromByteString (prov x) (toBytes x)

-- | Take the first 32 bytes and roll them into a Value, padding to the left
toValue :: ByteField -> Value
toValue bf = value (roll bs) $ readProvenance bf
  where bs = toBytes . padLeft 32 $ take 32 bf


-- Sublists
-- ---------------------------------------------------------------------

-- | Unsafe extract the first byte from a ByteField
head :: ByteField -> ByteP
head (ByteField (x:_) _) = x

-- | `take n bf` constructs a ByteField from (up to) the first `n` bytes of `bf`
take :: Int -> ByteField -> ByteField
take n (ByteField bps dp) = flip ByteField dp $ L.take n bps

-- | `drop n bf` constructs a ByteField excluding the first `n` bytes of `bf`
drop :: Int -> ByteField -> ByteField
drop n (ByteField bps (dp,o)) = flip ByteField (dp,o+n) $ L.drop n bps

-- | `slice i n` extracts up to `n` bytes starting at the `i`th
slice :: Int -> Int -> ByteField -> ByteField
slice i sz = take sz . drop i 

-- | `fullslice i n` extracts `n` bytes starting at the `i`th, possibly with null-padding if needed
fullslice :: Int -> Int -> ByteField -> ByteField
fullslice i n = padRight n . slice i n 

-- | Write the contents of a ByteField into another at an offset, overwriting and possibly extending the original
splice :: Int -> ByteField -> ByteField -> ByteField
splice i a b =
  let
    start = padRight i $ take i a
    end   = drop (i + size b) a
  in
    mconcat [start, b, end]

-- | `copy p1 p2 n a b` splices `n` bytes from `a` starting at `p1` to `b` starting at `p2`
copy :: Int -> Int -> Int -> ByteField -> ByteField -> ByteField
copy p1 p2 n a = splice p2 (fullslice p1 n a)


-- Reading and writing Values
-- ---------------------------------------------------------------------

-- | Read a 32-byte value from a position in the ByteField, padding with null bytes if necessary
getWord :: Int -> ByteField -> Value
getWord i bf = Value (roll $ toBytes w) (readProvenance w)
  where w = fullslice i 32 bf

-- | Write a 32-byte value to a position in the ByteField, possibly extending it
setWord :: Int -> Value -> ByteField -> ByteField
setWord i v = splice i $ fromValue v

-- | Write a single byte to the ByteField, uses the lower byte of the value
setByte :: Int -> Value -> ByteField -> ByteField
setByte i v = splice i $ drop 31 $ fromValue v

-- | Unsafely get the byte value at a particular index
getByteRaw :: Int -> ByteField -> Word8
getByteRaw i = bpVal . head . drop i

-- | `getBytesRaw i n` extracts a slice of up to `n` bytes starting from `i` as a raw ByteString
getBytesRaw :: Int -> Int -> ByteField -> ByteString
getBytesRaw i n = toBytes . slice i n


-- Provenance tracking
-- ---------------------------------------------------------------------

-- | Get the read-provenance of a ByteField, unchanged if the provenance, length and offset of each byte is unchanged
readProvenance :: ByteField -> Prov
readProvenance bf = case map fuseGroup $ provGroups bf of
  []                -> Nul
  (bs, (0, p)) : [] -> SliceRead bs p
--                       if B.length bs < B.length (valueAt p)
--                       then SliceRead bs p
--                       else p
  (bs, (o, p)) : [] -> TransRead bs o p
  xs                -> DirtyRead (B.concat $ map fst xs) (map snd xs)

-- | Unwraps a ByteString, taking the first non-null prov and (from the same byte) the lowest offset plus its index
fuseGroup :: ByteField -> (ByteString, (Int, Prov))
fuseGroup (ByteField xs dp) = 
  let
    firstNonNul   = L.findIndex ((/= Nul) . bpProv) xs
    (prov, offset) = case firstNonNul of
      Just i -> let b = L.head $ L.drop i xs
                in (bpProv b, bpOffset b + i)
      Nothing -> (Nul, 0)
  in
    (B.pack $ L.map bpVal xs, (offset, prov))

-- | Split a ByteField into groups of same provenance, where the Nul provenance matches anything
provGroups :: ByteField -> [ByteField]
provGroups (ByteField bps dp) = 
  let
    sameProv b1 b2 = case (bpProv b1, bpProv b2) of
      (Nul, _  ) -> True
      (_  , Nul) -> True
      (p1 , p2 ) -> p1 == p2
  in
    map (flip ByteField dp) $ L.groupBy sameProv bps


