{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Decode where

import Prelude hiding (LT, GT, fail)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Control.Monad.Loops (whileM)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Text.Printf (printf)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Control.DeepSeq

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (Structured(fmt), (~-))

import Fluidity.EVM.Types
import qualified Fluidity.EVM.Data as Data

type Decode = Resultant State Error

data State = State
  { stStream :: [Word8]
  , stPos    :: CAddr
  }

data Error
  = EndOfStream
  | InvalidOpcode Word8 CAddr
  | InvalidOffset Int
  deriving (Show, Generic, NFData)

instance Structured Error where
  fmt e = case e of
    EndOfStream       -> fmt "Unexpected end of file."
    InvalidOpcode x p -> "Invalid opcode" ~- (hex8 x) ~- "at byte" ~- p

decode :: ByteString -> Result Error Code
decode x = snd $ runResultant stream state
  where state = State (B.unpack x) (Data.mkInternal 0)

decode1 :: Int -> ByteString -> Result Error (Op, Int)
decode1 i bs =
  if
    i < 0 || i >= B.length bs
  then
    Err $ InvalidOffset i
  else 
    case sym . B.head $ B.drop i bs of
      Left (n, f) ->
        Ok (f . B.take n $ B.drop (i+1) bs, n+1)

      Right op ->
        Ok (op, 1)

decodeHex = decode . fst . B16.decode
 
stream :: Decode [Op]
stream = concat <$> whileM (not <$> finished) next

next :: Decode [Op]
next = do
  x <- pop
  let s = sym x
  case s of
    Right op@(Invalid _) -> do
      st <- getState
      setState $ st { stStream = [] }
      return [op]

    
    Right op -> 
      return [op]

    Left (n, f) -> do
      xs <- consume n
      let cdata = map (CData . toInteger) $ B.unpack xs
      return $ (f xs):cdata

finished :: Decode Bool
finished = ((==) 0 . length . stStream) <$> getState

pop :: Decode Word8
pop = do
  xs        <- stStream <$> getState
  p         <- stPos <$> getState
  (x : xs') <- if (length xs) > 0 then return xs
               else fail $ EndOfStream
  setState $ State
    { stStream = xs'
    , stPos    = Data.add p (Data.mkInternal 1) }
  return x

consume :: Int -> Decode ByteString
consume n = do
  xs        <- stStream <$> getState
  p         <- stPos <$> getState
  (xs', ys) <- if (length xs) >= n then return (splitAt n xs)
               else fail $ EndOfStream
  setState $ State
    { stStream = ys
    , stPos    = Data.add p (Data.mkInternal (toInteger n)) }
  return (B.pack xs')

sym :: Word8 -> Either (Int, ByteString -> Op) Op
sym op = case op of
  0x00 -> Right Stop          
  0x01 -> Right Add           
  0x02 -> Right Mul           
  0x03 -> Right Sub           
  0x04 -> Right Div           
  0x05 -> Right SDiv          
  0x06 -> Right Mod           
  0x07 -> Right SMod          
  0x08 -> Right AddMod        
  0x09 -> Right MulMod        
  0x0A -> Right Exp           
  0x0b -> Right SignExtend    
  0x10 -> Right LT            
  0x11 -> Right GT            
  0x12 -> Right SLT           
  0x13 -> Right SGT           
  0x14 -> Right Eq            
  0x15 -> Right IsZero        
  0x16 -> Right And           
  0x17 -> Right Or            
  0x18 -> Right Xor           
  0x19 -> Right Not           
  0x1a -> Right Byte          
  0x20 -> Right SHA3          
  0x30 -> Right Address       
  0x31 -> Right Balance       
  0x32 -> Right Origin        
  0x33 -> Right Caller        
  0x34 -> Right CallValue     
  0x35 -> Right CallDataLoad  
  0x36 -> Right CallDataSize  
  0x37 -> Right CallDataCopy  
  0x38 -> Right CodeSize      
  0x39 -> Right CodeCopy      
  0x3a -> Right GasPrice      
  0x3b -> Right ExtCodeSize   
  0x3c -> Right ExtCodeCopy   
  0x40 -> Right BlockHash     
  0x41 -> Right Coinbase      
  0x42 -> Right Timestamp     
  0x43 -> Right Number        
  0x44 -> Right Difficulty    
  0x45 -> Right GasLimit      
  0x50 -> Right Pop           
  0x51 -> Right MLoad         
  0x52 -> Right MStore        
  0x53 -> Right MStore8       
  0x54 -> Right SLoad         
  0x55 -> Right SStore        
  0x56 -> Right Jump          
  0x57 -> Right JumpI         
  0x58 -> Right PC            
  0x59 -> Right MSize         
  0x5a -> Right Gas           
  0x5b -> Right JumpDest      
  0x60 -> Left (1,  Push1)   
  0x61 -> Left (2,  Push2)   
  0x62 -> Left (3,  Push3)   
  0x63 -> Left (4,  Push4)   
  0x64 -> Left (5,  Push5)   
  0x65 -> Left (6,  Push6)   
  0x66 -> Left (7,  Push7)   
  0x67 -> Left (8,  Push8)   
  0x68 -> Left (9,  Push9)   
  0x69 -> Left (10, Push10)  
  0x6a -> Left (11, Push11)  
  0x6b -> Left (12, Push12)  
  0x6c -> Left (13, Push13)  
  0x6d -> Left (14, Push14)  
  0x6e -> Left (15, Push15)  
  0x6f -> Left (16, Push16)  
  0x70 -> Left (17, Push17)  
  0x71 -> Left (18, Push18)  
  0x72 -> Left (19, Push19)  
  0x73 -> Left (20, Push20)  
  0x74 -> Left (21, Push21)  
  0x75 -> Left (22, Push22)  
  0x76 -> Left (23, Push23)  
  0x77 -> Left (24, Push24)  
  0x78 -> Left (25, Push25)  
  0x79 -> Left (26, Push26)  
  0x7a -> Left (27, Push27)  
  0x7b -> Left (28, Push28)  
  0x7c -> Left (29, Push29)  
  0x7d -> Left (30, Push30)  
  0x7e -> Left (31, Push31)  
  0x7f -> Left (32, Push32)  
  0x80 -> Right Dup1          
  0x81 -> Right Dup2          
  0x82 -> Right Dup3          
  0x83 -> Right Dup4          
  0x84 -> Right Dup5          
  0x85 -> Right Dup6          
  0x86 -> Right Dup7          
  0x87 -> Right Dup8          
  0x88 -> Right Dup9          
  0x89 -> Right Dup10         
  0x8a -> Right Dup11         
  0x8b -> Right Dup12         
  0x8c -> Right Dup13         
  0x8d -> Right Dup14         
  0x8e -> Right Dup15         
  0x8f -> Right Dup16         
  0x90 -> Right Swap1         
  0x91 -> Right Swap2         
  0x92 -> Right Swap3         
  0x93 -> Right Swap4         
  0x94 -> Right Swap5         
  0x95 -> Right Swap6         
  0x96 -> Right Swap7         
  0x97 -> Right Swap8         
  0x98 -> Right Swap9         
  0x99 -> Right Swap10        
  0x9a -> Right Swap11        
  0x9b -> Right Swap12        
  0x9c -> Right Swap13        
  0x9d -> Right Swap14        
  0x9e -> Right Swap15        
  0x9f -> Right Swap16        
  0xa0 -> Right Log0          
  0xa1 -> Right Log1          
  0xa2 -> Right Log2          
  0xa3 -> Right Log3          
  0xa4 -> Right Log4          
  0xf0 -> Right Create        
  0xf1 -> Right Call          
  0xf2 -> Right CallCode      
  0xf3 -> Right Return        
  0xf4 -> Right DelegateCall  
  0xf5 -> Right Suicide       
  _    -> Right (Invalid $ toInteger op)

hex8 :: Word8 -> String
hex8 x = printf "0x%02x" x

hex16 :: Word16 -> String
hex16 x = printf "0x%04x" x

hex32 :: Word16 -> String
hex32 x = printf "0x%08x" x

hex64 :: Word64 -> String
hex64 x = printf "0x%16x" x


