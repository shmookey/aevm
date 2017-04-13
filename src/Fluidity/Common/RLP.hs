module Fluidity.Common.RLP where

import Prelude hiding (fail)
import Control.Monad.Loops (whileM)
import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.Word (Word8)
import qualified Data.ByteString as B

import Control.Monad.Result
import Control.Monad.Resultant

import Fluidity.Common.Binary (roll, unroll, toHex)


data RStruct = RItem ByteString | RList [RStruct]

data RLPError = NotEnoughBytes Int | EndOfInput Int | StructuralError
  deriving (Show)

instance Show RStruct where
  show (RItem bs) = "0x" ++ toHex bs
  show (RList xs) = "[" ++ (intercalate ", " (map show xs)) ++ "]"

class RLP a where
  toRLP   :: a -> RStruct
  fromRLP :: RStruct -> Result RLPError a

instance RLP RStruct where
  toRLP   = id
  fromRLP = return

instance RLP a => RLP [a] where
  toRLP       = RList . map toRLP
  fromRLP obj = case obj of
    RList xs -> mapM fromRLP xs
    _        -> Err StructuralError

instance RLP ByteString where
  toRLP = RItem
  fromRLP obj = case obj of
    RItem bs -> Ok bs
    _        -> Err StructuralError


-- Encoder
-- ---------------------------------------------------------------------

encode :: RLP a => a -> ByteString
encode obj = case toRLP obj of
  RItem bs -> 
    let sz   = B.length bs
        val  = B.head bs
        szbs = unroll sz
        sz'  = B.length szbs
    in if sz == 0                   then bs
       else if sz == 1 && val < 128 then bs
       else if sz < 56              then unroll (sz+128)  <> bs
       else                              unroll (sz'+183) <> szbs <> bs

  RList xs ->
    let bs   = mconcat $ map encode xs
        sz   = B.length bs
        szbs = unroll sz
        sz'  = B.length szbs
    in if sz < 56 then unroll (sz+192)  <> bs
       else            unroll (sz'+247) <> szbs <> bs
                  


-- Decoder
-- ---------------------------------------------------------------------

type Decode = Resultant (Int, ByteString) RLPError

decode :: ByteString -> Result RLPError RStruct
decode bs = snd $ runResultant struct (0,bs)

struct :: Decode RStruct
struct = do
  b <- fromIntegral <$> byte :: Decode Int
  if      b < 0x80 then RItem <$> (return . B.singleton $ fromIntegral b)
  else if b < 0xb8 then RItem <$>  bytes  (b - 0x80)
  else if b < 0xc0 then RItem <$> (len    (b - 0xb7) >>= bytes)
  else if b < 0xf8 then RList <$>  many   (b - 0xc0)
  else                  RList <$> (len    (b - 0xf7) >>= many)

many :: Int -> Decode [RStruct]
many n = do
  (pos, bs) <- getState
  bs' <- bytes n
  
  setState (pos, bs')
  xs <- whileM (not <$> finished) struct
  setState (pos + n, B.drop n bs)
  return xs

getPos :: Decode Int
getPos = fst <$> getState

remainder :: Decode ByteString
remainder = snd <$> getState

finished :: Decode Bool
finished = B.null <$> remainder

len :: Int -> Decode Int
len n = fromInteger . roll <$> bytes n

byte :: Decode Word8
byte = do
  (pos, bs) <- getState
  if B.length bs == 0
  then fail $ EndOfInput pos
  else setState (pos + 1, B.tail bs)
  return $ B.head bs

bytes :: Int -> Decode ByteString
bytes n = do
  (pos, bs) <- getState
  if B.length bs < n
  then fail $ NotEnoughBytes pos
  else return ()
  let (x, bs') = B.splitAt n bs
  setState (pos + n, bs')
  return x


