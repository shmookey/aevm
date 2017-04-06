{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Fluidity.EVM.Types where

import Prelude hiding (Value)
import Data.Text (Text)
import Data.Map (Map)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad.Resultant
import Control.Monad.Interruptible

import Fluidity.EVM.Data (Value, ByteField)
import qualified Fluidity.EVM.Data as Data


-- Fundamental types
type CAddr     = Value       -- Byte-addressed code pointer
type MAddr     = Value       -- Byte-addressed memory pointer
type SAddr     = Value       -- Value-addressed storage pointer
type CDAddr    = Value       -- Byte-addressed calldata pointer

-- State types
type PC         = CAddr       -- Program counter (code pointer)
type Stack      = [Value]      
type Code       = [Op]
type BlockHash  = Value
type TxHash     = Value
type Bytecode   = ByteString
type Memory     = ByteField
type Storage    = Map Value Value
type CallData   = ByteField
type ReturnData = ByteField
type Address    = Value
type Balance    = Value
type Gas        = Value

type ExtCall = (Gas, Address, Balance, ByteField, MAddr, Value)

data Log = Log ByteField [Value]
  deriving (Eq, Show, Generic, NFData)

-- Bytecode types
type Label     = Text
data Program   = Program [Op] deriving (Read, Show)
data SourceMap = SourceMap Program [Int] deriving (Read, Show)

data Op
  -- 00s: Stop and arithmetic operations
  = Stop          -- 0x00 Halt execution
  | Add           -- 0x01 Addition operation
  | Mul           -- 0x02 Multiplication operation
  | Sub           -- 0x03 Subtraction operation
  | Div           -- 0x04 Integer division operation
  | SDiv          -- 0x05 Signed integer division operation (truncated)
  | Mod           -- 0x06 Modulo remainder operation
  | SMod          -- 0x07 Signed modulo remainder operation
  | AddMod        -- 0x08 Modulo addition operation
  | MulMod        -- 0x09 Modulo multiplication operation
  | Exp           -- 0x0A Exponential operation
  | SignExtend    -- 0x0b Extend length of two's complement signed integer

  -- 10s: Comparison and bitwise logic operations
  | LT            -- 0x10 Less-than comparison
  | GT            -- 0x11 Greater-than comparison
  | SLT           -- 0x12 Signed less-than comparison
  | SGT           -- 0x13 Signed greater-than comparison
  | Eq            -- 0x14 Equality comparison
  | IsZero        -- 0x15 Simple "not" operator
  | And           -- 0x16 Bitwise AND operation
  | Or            -- 0x17 Bitwise OR operation
  | Xor           -- 0x18 Bitwise XOR operation
  | Not           -- 0x19 Bitwise NOT operation 
  | Byte          -- 0x1a Retrieve single byte from word
  
  -- 20s: SHA3
  | SHA3          -- 0x20 Compute Keccak-256 hash

  -- 30s: Environmental information
  | Address       -- 0x30 Get address of currently executing account
  | Balance       -- 0x31 Get balance of given account
  | Origin        -- 0x32 Get execution origination address
  | Caller        -- 0x33 Get caller address
  | CallValue     -- 0x34 Get deposited value by the instruction/transaction responsible for this execution
  | CallDataLoad  -- 0x35 Get input data of current environment
  | CallDataSize  -- 0x36 Get size of input data in current environment
  | CallDataCopy  -- 0x37 Copy input data of current environment to memory
  | CodeSize      -- 0x38 Get size of code running in current environment
  | CodeCopy      -- 0x39 Copy code running in current environment to memory
  | GasPrice      -- 0x3a Get price of gas in current environment
  | ExtCodeSize   -- 0x3b Get size of an account's code
  | ExtCodeCopy   -- 0x3c Copy an account’s code to memory

  -- 40s: Block information
  | BlockHash     -- 0x40 Get the hash of one of the 256 most recent complete blocks
  | Coinbase      -- 0x41 Get the block's beneficiary address
  | Timestamp     -- 0x42 Get the block's timestamp
  | Number        -- 0x43 Get the block's number
  | Difficulty    -- 0x44 Get the block's difficulty
  | GasLimit      -- 0x45 Get the block's gas limit

  -- 50s: Stack, memory, storage and flow operations
  | Pop           -- 0x50 Remove an item from stack
  | MLoad         -- 0x51 Load word from memory
  | MStore        -- 0x52 Save word to memory
  | MStore8       -- 0x53 Save byte to memory
  | SLoad         -- 0x54 Load word from storage
  | SStore        -- 0x55 Save word to storage
  | Jump          -- 0x56 Alter the program counter
  | JumpI         -- 0x57 Conditionally alter the program counter
  | PC            -- 0x58 Get the value of the program counter prior to the increment corresponding to this instruction
  | MSize         -- 0x59 Get the size of active memory in bytes
  | Gas           -- 0x5a Get the amount of available gas, including the corresponding reduction for the cost of this instruction
  | JumpDest      -- 0x5b Mark a valid destination for jumps

  -- 60s & 70s: Push operations
  | Push1  ByteString  -- 0x60 Place 1 byte item on stack
  | Push2  ByteString  -- 0x61 Place 2 byte item on stack
  | Push3  ByteString  -- 0x62 Place 3 byte item on stack
  | Push4  ByteString  -- 0x63 Place 4 byte item on stack
  | Push5  ByteString  -- 0x64 Place 5 byte item on stack
  | Push6  ByteString  -- 0x65 Place 6 byte item on stack
  | Push7  ByteString  -- 0x66 Place 7 byte item on stack
  | Push8  ByteString  -- 0x67 Place 8 byte item on stack
  | Push9  ByteString  -- 0x68 Place 9 byte item on stack
  | Push10 ByteString  -- 0x69 Place 10 byte item on stack
  | Push11 ByteString  -- 0x6a Place 11 byte item on stack
  | Push12 ByteString  -- 0x6b Place 12 byte item on stack
  | Push13 ByteString  -- 0x6c Place 13 byte item on stack
  | Push14 ByteString  -- 0x6d Place 14 byte item on stack
  | Push15 ByteString  -- 0x6e Place 15 byte item on stack
  | Push16 ByteString  -- 0x6f Place 16 byte item on stack
  | Push17 ByteString  -- 0x70 Place 17 byte item on stack
  | Push18 ByteString  -- 0x71 Place 18 byte item on stack
  | Push19 ByteString  -- 0x72 Place 19 byte item on stack
  | Push20 ByteString  -- 0x73 Place 20 byte item on stack
  | Push21 ByteString  -- 0x74 Place 21 byte item on stack
  | Push22 ByteString  -- 0x75 Place 22 byte item on stack
  | Push23 ByteString  -- 0x76 Place 23 byte item on stack
  | Push24 ByteString  -- 0x77 Place 24 byte item on stack
  | Push25 ByteString  -- 0x78 Place 25 byte item on stack
  | Push26 ByteString  -- 0x79 Place 26 byte item on stack
  | Push27 ByteString  -- 0x7a Place 27 byte item on stack
  | Push28 ByteString  -- 0x7b Place 28 byte item on stack
  | Push29 ByteString  -- 0x7c Place 29 byte item on stack
  | Push30 ByteString  -- 0x7d Place 30 byte item on stack
  | Push31 ByteString  -- 0x7e Place 31 byte item on stack
  | Push32 ByteString  -- 0x7f Place 32 byte item on stack

  -- 80s: Duplication operations
  | Dup1          -- 0x80 Duplicate 1st stack item
  | Dup2          -- 0x81 Duplicate 2nd stack item
  | Dup3          -- 0x82 Duplicate 3rd stack item
  | Dup4          -- 0x83 Duplicate 4th stack item
  | Dup5          -- 0x84 Duplicate 5th stack item
  | Dup6          -- 0x85 Duplicate 6th stack item
  | Dup7          -- 0x86 Duplicate 7th stack item
  | Dup8          -- 0x87 Duplicate 8th stack item
  | Dup9          -- 0x88 Duplicate 9th stack item
  | Dup10         -- 0x89 Duplicate 10th stack item
  | Dup11         -- 0x8a Duplicate 11th stack item
  | Dup12         -- 0x8b Duplicate 12th stack item
  | Dup13         -- 0x8c Duplicate 13th stack item
  | Dup14         -- 0x8d Duplicate 14th stack item
  | Dup15         -- 0x8e Duplicate 15th stack item
  | Dup16         -- 0x8f Duplicate 16th stack item

  -- 80s: Exchange operations
  | Swap1         -- 0x90 Exchange 1st and 2nd stack items
  | Swap2         -- 0x91 Exchange 1st and 3rd stack items
  | Swap3         -- 0x92 Exchange 1st and 4th stack items
  | Swap4         -- 0x93 Exchange 1st and 5th stack items
  | Swap5         -- 0x94 Exchange 1st and 6th stack items
  | Swap6         -- 0x95 Exchange 1st and 7th stack items
  | Swap7         -- 0x96 Exchange 1st and 8th stack items
  | Swap8         -- 0x97 Exchange 1st and 9th stack items
  | Swap9         -- 0x98 Exchange 1st and 10th stack items
  | Swap10        -- 0x99 Exchange 1st and 11th stack items
  | Swap11        -- 0x9a Exchange 1st and 12th stack items
  | Swap12        -- 0x9b Exchange 1st and 13th stack items
  | Swap13        -- 0x9c Exchange 1st and 14th stack items
  | Swap14        -- 0x9d Exchange 1st and 15th stack items
  | Swap15        -- 0x9e Exchange 1st and 16th stack items
  | Swap16        -- 0x9f Exchange 1st and 17th stack items

  -- a0s: Logging operations
  | Log0          -- 0xa0 Append log record with no topics
  | Log1          -- 0xa1 Append log record with 1 topic
  | Log2          -- 0xa2 Append log record with 2 topics
  | Log3          -- 0xa3 Append log record with 3 topics
  | Log4          -- 0xa4 Append log record with 4 topics

  -- f0s: System operations
  | Create        -- 0xf0 Create a new account with associated code
  | Call          -- 0xf1 Message-call into an account
  | CallCode      -- 0xf2 Message-call into this account with alternative account's code
  | Return        -- 0xf3 Halt execution returning output data
  | DelegateCall  -- 0xf4 Message-call into this account with an alternative account's code, but persisting the current values for sender and value
  | Suicide       -- 0xf5 Halt execution and register account for later deletion

  -- Meta-opcodes
  | PushLabel Label
  | Label     Label
  | Comment   Text
  | CData     Integer
  | Invalid   Integer
  deriving (Eq, Read, Show, Generic, NFData)

