module Fluidity.EVM.Data.Format where

import Prelude hiding (truncate)
import Data.List (dropWhileEnd)
import Data.ByteString (ByteString)
import System.Console.ANSI
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8

import Text.Structured

import Fluidity.Common.ANSI
import Fluidity.Common.Binary
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField (ByteField)


instance Structured Value where
  fmt = fmt . show

instance Structured ByteField where
  fmt = fmt . show


-- | Format a value in hexadecimal with no padding
toHexShort :: Value -> String
toHexShort v = "0x" ++ (B8.unpack . B16.encode $ bytes v)

-- | Format a hex stub from the upper 4 bytes of a value
stub :: Bytes a => a -> String
stub = take 8 . toHex

-- | Format a hex stub from the upper 4 bytes of an address
stubAddress :: Bytes a => a -> String
stubAddress = toString . colour Magenta . ("0x" ++) . take 8
  . B8.unpack . B16.encode . padBytes 20 . truncate 20 . toBytes

-- | Format a hex stub from the lower 4 bytes of a value
stubEnd :: Value -> String
stubEnd = drop 56 . toHex

codeptr :: Value -> String
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

abbreviated :: ByteString -> String
abbreviated bs =  
  let hs  = toHex bs
      len = length hs
  in (++) "0x" $ 
     if len > 12
     then take 12 hs ++ "{+" ++ show (len-12) ++ "b}"
     else hs

fmtCodePtr :: Int -> Fragment
fmtCodePtr x = highlight Magenta $ "0x" ~~ (toHexWord 2 x :: String)

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

