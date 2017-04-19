module Fluidity.EVM.Data.Format where

import Prelude hiding (truncate)
import qualified Prelude as P
import Data.List (dropWhileEnd)
import Data.ByteString (ByteString)
import System.Console.ANSI
import qualified Data.List.Split as Split
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B

import Text.Structured hiding (indent)

import Fluidity.Common.ANSI
import Fluidity.Common.Binary
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField (ByteField, ByteP)
import qualified Fluidity.EVM.Data.Prov as Prov
import qualified Fluidity.EVM.Data.ByteField as BF


instance Structured Value where
  fmt = fmt . show

instance Structured ByteField where
  fmt = fmt . show


-- | Format a value in hexadecimal with no padding
toHexShort :: Value -> String
toHexShort v = "0x" ++ (B8.unpack . B16.encode $ bytes v)

-- | Format a hex stub from the upper 4 bytes of a value
stub :: Bytes a => a -> String
stub = toHex . B.take 4 . toWord 32 . toBytes

stubN :: Bytes a => Int -> a -> String
stubN x = toHex . B.take x . toWord 32 . toBytes

-- | Format a hex stub from the upper 4 bytes of an address
stubAddress :: Bytes a => a -> String
stubAddress = toString . colour Magenta . ("0x" ++) . take 8
  . B8.unpack . B16.encode . padBytes 20 . truncate 20 . toBytes

-- | Format a hex stub from the lower 4 bytes of a value
stubEnd :: Bytes a => a -> String
stubEnd = toHex . B.drop 28 . toWord 32 . toBytes

stubEndN :: Bytes a => Int -> a -> String
stubEndN n = toHex . B.drop (32 - n) . toWord 32 . toBytes

-- | e.g. a38e..9f2b
stubRange :: Bytes a => a -> String
stubRange v = 
  let x = toHex v
  in if length x <= 8 then x
     else take 4 x ++ ".." ++ drop (length x - 4) x

-- | e.g. a38e..(39)..9f2b
stubRangeLen :: Bytes a => a -> String
stubRangeLen v = 
  let x = toHex v
      n = B.length $ toBytes v
  in if length x <= 8 then x
     else take 4 x ++ "..(" ++ show n ++ ").." ++ drop (length x - 4) x

codeptr :: Bytes a => a -> String
codeptr x = "0x" ++ (drop 4 $ stubEnd x)

-- | Like `address` but takes a ByteString and truncates to 20 bytes
address :: Bytes a => a -> String
address = toString . colour Magenta . ("0x" ++) . B8.unpack . B16.encode . padBytes 20 . truncate 20 . toBytes

-- | Format a a value as a currency amount
currency :: Value -> String
currency v = toString . colour Green $
  let
    x = uint v
    b = show x
  in
    if length b > 12     then insertDecimal 18 6 b ++ " ether"
    else if length b > 6 then insertDecimal 6  6 b ++ " szabo"
    else                      b                    ++ " wei"

boolean :: Value -> String
boolean v = if int v == 0 then "false" else "true"

smartStub :: Bytes a => a -> String
smartStub v = 
  let
    x = trim $ toBytes v
    len = B8.length x
  in
    if len == 20 -- probably an address
    then address x
    else if len < 20 && len /= 4
    then show $ roll x
    else if len == 32 || len == 31
    then stubRange v
    else stubRangeLen x

-- | Try to guess what type of value it is, and display it appropriately
smart :: Value -> String
smart v = 
  let
    len = B8.length $ bytes v
  in
    if len == 20 -- probably an address
    then address v
    else if len < 20
    then show $ uint v
    else toHex v
    
-- | Try to guess what type of value it is, and display it appropriately
smart' :: ByteString -> String
smart' bs = 
  let
    len = B8.length bs
  in
    if len == 20 -- probably an address
    then address bs
    else if len < 20
    then show $ roll bs
    else "0x" ++ toHex bs

-- | Try to guess what type of value it is, and display it appropriately
shortSmart :: Int -> ByteString -> String
shortSmart sz bs = 
  let
    len = B8.length bs
  in
    if len == 20 -- probably an address
    then address bs
    else if len < 20
    then show $ roll bs
    else abbreviated sz bs

abbreviated :: Int -> ByteString -> String
abbreviated n bs =  
  let hs  = toHex $ padBytes 1 bs
      len = length hs
  in (++) "0x" $ 
     if len > n
     then take n hs ++ "{+" ++ show (len-n) ++ "b}"
     else hs

fmtCodePtr :: Int -> Fragment
fmtCodePtr x = highlight Magenta $ "0x" ~~ (toHexWord 2 x :: String)

-- Long-form display functions
-- --------------------------------------------------------------------

accountDetails :: ByteString -> Account -> ByteField -> String
accountDetails addr (Account bal hash storage) code = toString $
  address addr                                    ~~ "\n" ~~
  "balance:   " ~~ currency bal                   ~~ "\n" ~~
  "code hash: " ~~ (toHex hash :: String)         ~~ "\n" ~~
  "code:      " ~~ abbreviated 64 (toBytes code)  ~~ "\n" ~~
  "storage: \n" ~~ indent 4 (storageDump storage) ~~ "\n"

storageDump :: StorageDB -> String
storageDump = P.unlines . map line . storageEntriesDB
  where line (k,v) = smart k ++ " " ++ smart v

-- Utility functions
-- ---------------------------------------------------------------------

insertDecimal :: Int -> Int -> String -> String
insertDecimal n r xs =
  let
    sz      = length xs
    padding = take (n - sz) $ repeat '0'
    left    = take (sz - n) xs
    right   = dropWhileEnd (== '0') . take r $ drop (sz - n) xs
  in if n >= sz
     then "0." ++ (dropWhileEnd (== '0') $ take r (padding ++ xs))
     else if length right == 0 then left
     else left ++ "." ++ (dropWhileEnd (== '0') right)

indent :: Int -> String -> String
indent n = P.unlines . map (pad ++) . lines
  where pad = take n $ repeat ' '

