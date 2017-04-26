{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Bytecode where

import Prelude hiding (LT, GT)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Control.Monad.Result

import Fluidity.Common.Binary (toBytes, toWord, unroll)
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField
import qualified Fluidity.EVM.Data.Prov as Prov

data Error
  = OutOfBounds Int Int 
  | EndOfInput
  deriving (Eq, Show, Generic, NFData)


getOp :: Int -> ByteField -> Result Error (Int, Op)
getOp p bs =
  if p >= 0 && p < size bs
  then case sym $ getByteRaw p bs of
    Right op     -> Ok (1, op)
    Left (n, op) -> getData (p+1) n bs >>= Ok . (,) (n+1) . op
  else
    Err $ OutOfBounds p (size bs)

getData :: Int -> Int -> ByteField -> Result Error Value
getData p n bs =
  if p >= 0 && p + n <= size bs
  then 
    let v = toValue $ slice p n bs
    in Ok $
      if n == 32 then v
      else addProv (Prov.Fit $ toBytes v) v
  else
    Err EndOfInput

disassemble :: ByteField -> [Op]
disassemble bf = readOps 0
  where readOps i = 
          if i >= size bf then [] 
          else case getOp i bf of
            Ok (n, x) -> x : readOps (n+i)
            Err e     -> error $ "internal error in disasm: " ++ show e

disassembleWithPtr :: ByteField -> [(Int, Op)]
disassembleWithPtr bf = readOps 0
  where readOps i = 
          if i >= size bf then [] 
          else case getOp i bf of
            Ok (n, x) -> (i, x) : readOps (n+i)
            Err e     -> error $ "internal error in disasm: " ++ show e

assemble :: [Op] -> ByteField
assemble ops = fromByteString Prov.Nul . B.concat $ map toSym ops

data Op
  -- 00s: Stop and arithmetic operations
  = Stop               -- 0x00 Halt execution
  | Add                -- 0x01 Addition operation
  | Mul                -- 0x02 Multiplication operation
  | Sub                -- 0x03 Subtraction operation
  | Div                -- 0x04 Integer division operation
  | SDiv               -- 0x05 Signed integer division operation (truncated)
  | Mod                -- 0x06 Modulo remainder operation
  | SMod               -- 0x07 Signed modulo remainder operation
  | AddMod             -- 0x08 Modulo addition operation
  | MulMod             -- 0x09 Modulo multiplication operation
  | Exp                -- 0x0A Exponential operation
  | SignExtend         -- 0x0b Extend length of two's complement signed integer

  -- 10s: Comparison and bitwise logic operations
  | LT                 -- 0x10 Less-than comparison
  | GT                 -- 0x11 Greater-than comparison
  | SLT                -- 0x12 Signed less-than comparison
  | SGT                -- 0x13 Signed greater-than comparison
  | Eq                 -- 0x14 Equality comparison
  | IsZero             -- 0x15 Simple "not" operator
  | And                -- 0x16 Bitwise AND operation
  | Or                 -- 0x17 Bitwise OR operation
  | Xor                -- 0x18 Bitwise XOR operation
  | Not                -- 0x19 Bitwise NOT operation 
  | Byte               -- 0x1a Retrieve single byte from word
  
  -- 20s: SHA3
  | SHA3               -- 0x20 Compute Keccak-256 hash

  -- 30s: Environmental information
  | Address            -- 0x30 Get address of currently executing account
  | Balance            -- 0x31 Get balance of given account
  | Origin             -- 0x32 Get execution origination address
  | Caller             -- 0x33 Get caller address
  | CallValue          -- 0x34 Get deposited value by the instruction/transaction responsible for this execution
  | CallDataLoad       -- 0x35 Get input data of current environment
  | CallDataSize       -- 0x36 Get size of input data in current environment
  | CallDataCopy       -- 0x37 Copy input data of current environment to memory
  | CodeSize           -- 0x38 Get size of code running in current environment
  | CodeCopy           -- 0x39 Copy code running in current environment to memory
  | GasPrice           -- 0x3a Get price of gas in current environment
  | ExtCodeSize        -- 0x3b Get size of an account's code
  | ExtCodeCopy        -- 0x3c Copy an account’s code to memory

  -- 40s: Block information
  | BlockHash          -- 0x40 Get the hash of one of the 256 most recent complete blocks
  | Coinbase           -- 0x41 Get the block's beneficiary address
  | Timestamp          -- 0x42 Get the block's timestamp
  | Number             -- 0x43 Get the block's number
  | Difficulty         -- 0x44 Get the block's difficulty
  | GasLimit           -- 0x45 Get the block's gas limit

  -- 50s: Stack, memory, storage and flow operations
  | Pop                -- 0x50 Remove an item from stack
  | MLoad              -- 0x51 Load word from memory
  | MStore             -- 0x52 Save word to memory
  | MStore8            -- 0x53 Save byte to memory
  | SLoad              -- 0x54 Load word from storage
  | SStore             -- 0x55 Save word to storage
  | Jump               -- 0x56 Alter the program counter
  | JumpI              -- 0x57 Conditionally alter the program counter
  | PC                 -- 0x58 Get the value of the program counter prior to the increment corresponding to this instruction
  | MSize              -- 0x59 Get the size of active memory in bytes
  | Gas                -- 0x5a Get the amount of available gas, including the corresponding reduction for the cost of this instruction
  | JumpDest           -- 0x5b Mark a valid destination for jumps

  -- 60s & 70s: Push operations
  | Push1  Value       -- 0x60 Place 1 byte item on stack
  | Push2  Value       -- 0x61 Place 2 byte item on stack
  | Push3  Value       -- 0x62 Place 3 byte item on stack
  | Push4  Value       -- 0x63 Place 4 byte item on stack
  | Push5  Value       -- 0x64 Place 5 byte item on stack
  | Push6  Value       -- 0x65 Place 6 byte item on stack
  | Push7  Value       -- 0x66 Place 7 byte item on stack
  | Push8  Value       -- 0x67 Place 8 byte item on stack
  | Push9  Value       -- 0x68 Place 9 byte item on stack
  | Push10 Value       -- 0x69 Place 10 byte item on stack
  | Push11 Value       -- 0x6a Place 11 byte item on stack
  | Push12 Value       -- 0x6b Place 12 byte item on stack
  | Push13 Value       -- 0x6c Place 13 byte item on stack
  | Push14 Value       -- 0x6d Place 14 byte item on stack
  | Push15 Value       -- 0x6e Place 15 byte item on stack
  | Push16 Value       -- 0x6f Place 16 byte item on stack
  | Push17 Value       -- 0x70 Place 17 byte item on stack
  | Push18 Value       -- 0x71 Place 18 byte item on stack
  | Push19 Value       -- 0x72 Place 19 byte item on stack
  | Push20 Value       -- 0x73 Place 20 byte item on stack
  | Push21 Value       -- 0x74 Place 21 byte item on stack
  | Push22 Value       -- 0x75 Place 22 byte item on stack
  | Push23 Value       -- 0x76 Place 23 byte item on stack
  | Push24 Value       -- 0x77 Place 24 byte item on stack
  | Push25 Value       -- 0x78 Place 25 byte item on stack
  | Push26 Value       -- 0x79 Place 26 byte item on stack
  | Push27 Value       -- 0x7a Place 27 byte item on stack
  | Push28 Value       -- 0x7b Place 28 byte item on stack
  | Push29 Value       -- 0x7c Place 29 byte item on stack
  | Push30 Value       -- 0x7d Place 30 byte item on stack
  | Push31 Value       -- 0x7e Place 31 byte item on stack
  | Push32 Value       -- 0x7f Place 32 byte item on stack

  -- 80s: Duplication operations
  | Dup1               -- 0x80 Duplicate 1st stack item
  | Dup2               -- 0x81 Duplicate 2nd stack item
  | Dup3               -- 0x82 Duplicate 3rd stack item
  | Dup4               -- 0x83 Duplicate 4th stack item
  | Dup5               -- 0x84 Duplicate 5th stack item
  | Dup6               -- 0x85 Duplicate 6th stack item
  | Dup7               -- 0x86 Duplicate 7th stack item
  | Dup8               -- 0x87 Duplicate 8th stack item
  | Dup9               -- 0x88 Duplicate 9th stack item
  | Dup10              -- 0x89 Duplicate 10th stack item
  | Dup11              -- 0x8a Duplicate 11th stack item
  | Dup12              -- 0x8b Duplicate 12th stack item
  | Dup13              -- 0x8c Duplicate 13th stack item
  | Dup14              -- 0x8d Duplicate 14th stack item
  | Dup15              -- 0x8e Duplicate 15th stack item
  | Dup16              -- 0x8f Duplicate 16th stack item

  -- 80s: Exchange operations
  | Swap1              -- 0x90 Exchange 1st and 2nd stack items
  | Swap2              -- 0x91 Exchange 1st and 3rd stack items
  | Swap3              -- 0x92 Exchange 1st and 4th stack items
  | Swap4              -- 0x93 Exchange 1st and 5th stack items
  | Swap5              -- 0x94 Exchange 1st and 6th stack items
  | Swap6              -- 0x95 Exchange 1st and 7th stack items
  | Swap7              -- 0x96 Exchange 1st and 8th stack items
  | Swap8              -- 0x97 Exchange 1st and 9th stack items
  | Swap9              -- 0x98 Exchange 1st and 10th stack items
  | Swap10             -- 0x99 Exchange 1st and 11th stack items
  | Swap11             -- 0x9a Exchange 1st and 12th stack items
  | Swap12             -- 0x9b Exchange 1st and 13th stack items
  | Swap13             -- 0x9c Exchange 1st and 14th stack items
  | Swap14             -- 0x9d Exchange 1st and 15th stack items
  | Swap15             -- 0x9e Exchange 1st and 16th stack items
  | Swap16             -- 0x9f Exchange 1st and 17th stack items

  -- a0s: Logging operations
  | Log0               -- 0xa0 Append log record with no topics
  | Log1               -- 0xa1 Append log record with 1 topic
  | Log2               -- 0xa2 Append log record with 2 topics
  | Log3               -- 0xa3 Append log record with 3 topics
  | Log4               -- 0xa4 Append log record with 4 topics

  -- f0s: System operations
  | Create             -- 0xf0 Create a new account with associated code
  | Call               -- 0xf1 Message-call into an account
  | CallCode           -- 0xf2 Message-call into this account with alternative account's code
  | Return             -- 0xf3 Halt execution returning output data
  | DelegateCall       -- 0xf4 Message-call into this account with an alternative account's code, but persisting the current values for sender and value
  | Suicide            -- 0xf5 Halt execution and register account for later deletion

  | Invalid Integer
  deriving (Eq, Show, Generic, NFData)

sym :: Word8 -> Either (Int, Value -> Op) Op
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

toSym :: Op -> ByteString
toSym op = case op of
   Stop         -> B.singleton 0x00 
   Add          -> B.singleton 0x01 
   Mul          -> B.singleton 0x02 
   Sub          -> B.singleton 0x03 
   Div          -> B.singleton 0x04 
   SDiv         -> B.singleton 0x05 
   Mod          -> B.singleton 0x06 
   SMod         -> B.singleton 0x07 
   AddMod       -> B.singleton 0x08 
   MulMod       -> B.singleton 0x09 
   Exp          -> B.singleton 0x0A 
   SignExtend   -> B.singleton 0x0b 
   LT           -> B.singleton 0x10 
   GT           -> B.singleton 0x11 
   SLT          -> B.singleton 0x12 
   SGT          -> B.singleton 0x13 
   Eq           -> B.singleton 0x14 
   IsZero       -> B.singleton 0x15 
   And          -> B.singleton 0x16 
   Or           -> B.singleton 0x17 
   Xor          -> B.singleton 0x18 
   Not          -> B.singleton 0x19 
   Byte         -> B.singleton 0x1a 
   SHA3         -> B.singleton 0x20 
   Address      -> B.singleton 0x30 
   Balance      -> B.singleton 0x31 
   Origin       -> B.singleton 0x32 
   Caller       -> B.singleton 0x33 
   CallValue    -> B.singleton 0x34 
   CallDataLoad -> B.singleton 0x35 
   CallDataSize -> B.singleton 0x36 
   CallDataCopy -> B.singleton 0x37 
   CodeSize     -> B.singleton 0x38 
   CodeCopy     -> B.singleton 0x39 
   GasPrice     -> B.singleton 0x3a 
   ExtCodeSize  -> B.singleton 0x3b 
   ExtCodeCopy  -> B.singleton 0x3c 
   BlockHash    -> B.singleton 0x40 
   Coinbase     -> B.singleton 0x41 
   Timestamp    -> B.singleton 0x42 
   Number       -> B.singleton 0x43 
   Difficulty   -> B.singleton 0x44 
   GasLimit     -> B.singleton 0x45 
   Pop          -> B.singleton 0x50 
   MLoad        -> B.singleton 0x51 
   MStore       -> B.singleton 0x52 
   MStore8      -> B.singleton 0x53 
   SLoad        -> B.singleton 0x54 
   SStore       -> B.singleton 0x55 
   Jump         -> B.singleton 0x56 
   JumpI        -> B.singleton 0x57 
   PC           -> B.singleton 0x58 
   MSize        -> B.singleton 0x59 
   Gas          -> B.singleton 0x5a 
   JumpDest     -> B.singleton 0x5b 
   Push1  x     -> B.cons 0x60 $ toWord 1  x 
   Push2  x     -> B.cons 0x61 $ toWord 2  x 
   Push3  x     -> B.cons 0x62 $ toWord 3  x 
   Push4  x     -> B.cons 0x63 $ toWord 4  x 
   Push5  x     -> B.cons 0x64 $ toWord 5  x 
   Push6  x     -> B.cons 0x65 $ toWord 6  x 
   Push7  x     -> B.cons 0x66 $ toWord 7  x 
   Push8  x     -> B.cons 0x67 $ toWord 8  x 
   Push9  x     -> B.cons 0x68 $ toWord 9  x 
   Push10 x     -> B.cons 0x69 $ toWord 10 x 
   Push11 x     -> B.cons 0x6a $ toWord 11 x 
   Push12 x     -> B.cons 0x6b $ toWord 12 x 
   Push13 x     -> B.cons 0x6c $ toWord 13 x 
   Push14 x     -> B.cons 0x6d $ toWord 14 x 
   Push15 x     -> B.cons 0x6e $ toWord 15 x 
   Push16 x     -> B.cons 0x6f $ toWord 16 x 
   Push17 x     -> B.cons 0x70 $ toWord 17 x 
   Push18 x     -> B.cons 0x71 $ toWord 18 x 
   Push19 x     -> B.cons 0x72 $ toWord 19 x 
   Push20 x     -> B.cons 0x73 $ toWord 20 x 
   Push21 x     -> B.cons 0x74 $ toWord 21 x 
   Push22 x     -> B.cons 0x75 $ toWord 22 x 
   Push23 x     -> B.cons 0x76 $ toWord 23 x 
   Push24 x     -> B.cons 0x77 $ toWord 24 x 
   Push25 x     -> B.cons 0x78 $ toWord 25 x 
   Push26 x     -> B.cons 0x79 $ toWord 26 x 
   Push27 x     -> B.cons 0x7a $ toWord 27 x 
   Push28 x     -> B.cons 0x7b $ toWord 28 x 
   Push29 x     -> B.cons 0x7c $ toWord 29 x 
   Push30 x     -> B.cons 0x7d $ toWord 30 x 
   Push31 x     -> B.cons 0x7e $ toWord 31 x 
   Push32 x     -> B.cons 0x7f $ toWord 32 x 
   Dup1         -> B.singleton 0x80 
   Dup2         -> B.singleton 0x81 
   Dup3         -> B.singleton 0x82 
   Dup4         -> B.singleton 0x83 
   Dup5         -> B.singleton 0x84 
   Dup6         -> B.singleton 0x85 
   Dup7         -> B.singleton 0x86 
   Dup8         -> B.singleton 0x87 
   Dup9         -> B.singleton 0x88 
   Dup10        -> B.singleton 0x89 
   Dup11        -> B.singleton 0x8a 
   Dup12        -> B.singleton 0x8b 
   Dup13        -> B.singleton 0x8c 
   Dup14        -> B.singleton 0x8d 
   Dup15        -> B.singleton 0x8e 
   Dup16        -> B.singleton 0x8f 
   Swap1        -> B.singleton 0x90 
   Swap2        -> B.singleton 0x91 
   Swap3        -> B.singleton 0x92 
   Swap4        -> B.singleton 0x93 
   Swap5        -> B.singleton 0x94 
   Swap6        -> B.singleton 0x95 
   Swap7        -> B.singleton 0x96 
   Swap8        -> B.singleton 0x97 
   Swap9        -> B.singleton 0x98 
   Swap10       -> B.singleton 0x99 
   Swap11       -> B.singleton 0x9a 
   Swap12       -> B.singleton 0x9b 
   Swap13       -> B.singleton 0x9c 
   Swap14       -> B.singleton 0x9d 
   Swap15       -> B.singleton 0x9e 
   Swap16       -> B.singleton 0x9f 
   Log0         -> B.singleton 0xa0 
   Log1         -> B.singleton 0xa1 
   Log2         -> B.singleton 0xa2 
   Log3         -> B.singleton 0xa3 
   Log4         -> B.singleton 0xa4 
   Create       -> B.singleton 0xf0 
   Call         -> B.singleton 0xf1 
   CallCode     -> B.singleton 0xf2 
   Return       -> B.singleton 0xf3 
   DelegateCall -> B.singleton 0xf4 
   Suicide      -> B.singleton 0xf5 
   Invalid x    -> unroll x

