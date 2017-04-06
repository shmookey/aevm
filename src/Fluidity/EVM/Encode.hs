module Fluidity.EVM.Encode where

import Prelude hiding (GT, LT, fail)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Word (Word8)

import Control.Monad.Result
import Control.Monad.Resultant

import Fluidity.EVM.Binary
import Fluidity.EVM.Bytecode


{- EVM binary encoder

  This module contains functions for converting `Op` values into binary EVM
  opcodes. It also performs the task of replacing the labels inside `Label` and
  `PushLabel` meta-opcodes into their final byte offset addresses. In this
  sense, the Encoder also acts as a kind of final-stage assembler.
-}

-- Encoder monad
-- ---------------------------------------------------------------------

type Encode = Resultant State Error

-- | Track the locations of labels and references to them
data State = State
  { stPos    :: CodePtr
  , stLabels :: Map Label CodePtr  -- Locations where labelled sections begin
  , stRefs   :: [(CodePtr, Label)] -- Locations of label references
  }

data Error = UnresolvedReference Label
  deriving (Show)


run :: Encode a -> Result Error a
run m = snd $ runResultant m initState

initState :: State
initState = State 0 mempty []


-- Main encoding logic
-- ---------------------------------------------------------------------

-- | Fully encode a program, including label elimination.
encode :: Program -> Encode ByteString
encode (Program xs) = do
  code  <- B.concat <$> mapM encodeOp xs
  refs  <- stRefs <$> getState
  code' <- foldM fixRef code refs
  return code'

-- | Encode a program, generating a line-to-byte source map.
encodeMap :: Program -> Encode (ByteString, SourceMap)
encodeMap p@(Program xs) = 
  let
    getOffsets :: [ByteString] -> [Int]
    getOffsets = scanl (\acc x -> acc + (B.length x)) 0

    sourceMap :: [ByteString] -> SourceMap
    sourceMap = SourceMap p . getOffsets
  in do
    encs <- mapM encodeOp xs
    refs <- stRefs <$> getState
    code <- foldM fixRef (B.concat encs) refs
    return (code, sourceMap encs)

fixRef :: ByteString -> (CodePtr, Label) -> Encode ByteString
fixRef bs (i, x) = 
  let 
    i'     = fromIntegral (toInteger i)
    (l, r) = B.splitAt i' bs
  in do
    lbls   <- stLabels <$> getState
    p      <- fromMaybe (UnresolvedReference x) (Map.lookup x lbls)
    return $ B.concat [l, addrBytes p, B.drop 4 r]

addrBytes :: CodePtr -> ByteString
addrBytes = padBytes 4 . unroll

-- | Encode an operation into its bytecode representation
encodeOp :: Op -> Encode ByteString
encodeOp op = 
  let
    single :: Word8 -> Encode ByteString
    single x = do 
      updateState $ \st -> st { stPos = (stPos st) + 1 }
      return $ B.singleton x

    inline :: Word8 -> Int -> ByteString -> Encode ByteString
    inline x n xs =
      let bs  = B.cons x (padBytes n xs)
          inc = fromIntegral (toInteger (n + 1)) :: CodePtr
      in do updateState $ \st -> st { stPos = (stPos st) + inc }
            return bs
                  
  in case op of
    Stop          -> single 0x00
    Add           -> single 0x01 
    Mul           -> single 0x02 
    Sub           -> single 0x03 
    Div           -> single 0x04 
    SDiv          -> single 0x05 
    Mod           -> single 0x06 
    SMod          -> single 0x07 
    AddMod        -> single 0x08 
    MulMod        -> single 0x09 
    Exp           -> single 0x0A 
    SignExtend    -> single 0x0b 
    LT            -> single 0x10 
    GT            -> single 0x11 
    SLT           -> single 0x12 
    SGT           -> single 0x13 
    Eq            -> single 0x14 
    IsZero        -> single 0x15 
    And           -> single 0x16 
    Or            -> single 0x17 
    Xor           -> single 0x18 
    Not           -> single 0x19 
    Byte          -> single 0x1a 
    SHA3          -> single 0x20 
    Address       -> single 0x30 
    Balance       -> single 0x31 
    Origin        -> single 0x32 
    Caller        -> single 0x33 
    CallValue     -> single 0x34 
    CallDataLoad  -> single 0x35 
    CallDataSize  -> single 0x36 
    CallDataCopy  -> single 0x37 
    CodeSize      -> single 0x38 
    CodeCopy      -> single 0x39 
    GasPrice      -> single 0x3a 
    ExtCodeSize   -> single 0x3b 
    ExtCodeCopy   -> single 0x3c 
    BlockHash     -> single 0x40 
    Coinbase      -> single 0x41 
    Timestamp     -> single 0x42 
    Number        -> single 0x43 
    Difficulty    -> single 0x44 
    GasLimit      -> single 0x45 
    Pop           -> single 0x50 
    MLoad         -> single 0x51 
    MStore        -> single 0x52 
    MStore8       -> single 0x53 
    SLoad         -> single 0x54 
    SStore        -> single 0x55 
    Jump          -> single 0x56 
    JumpI         -> single 0x57 
    PC            -> single 0x58 
    MSize         -> single 0x59 
    Gas           -> single 0x5a 
    JumpDest      -> single 0x5b 
    Push1  x      -> inline 0x60 1  x
    Push2  x      -> inline 0x61 2  x 
    Push3  x      -> inline 0x62 3  x 
    Push4  x      -> inline 0x63 4  x 
    Push5  x      -> inline 0x64 5  x 
    Push6  x      -> inline 0x65 6  x 
    Push7  x      -> inline 0x66 7  x 
    Push8  x      -> inline 0x67 8  x 
    Push9  x      -> inline 0x68 9  x 
    Push10 x      -> inline 0x69 10 x 
    Push11 x      -> inline 0x6a 11 x 
    Push12 x      -> inline 0x6b 12 x 
    Push13 x      -> inline 0x6c 13 x 
    Push14 x      -> inline 0x6d 14 x 
    Push15 x      -> inline 0x6e 15 x 
    Push16 x      -> inline 0x6f 16 x 
    Push17 x      -> inline 0x70 17 x 
    Push18 x      -> inline 0x71 18 x 
    Push19 x      -> inline 0x72 19 x 
    Push20 x      -> inline 0x73 20 x 
    Push21 x      -> inline 0x74 21 x 
    Push22 x      -> inline 0x75 22 x 
    Push23 x      -> inline 0x76 23 x 
    Push24 x      -> inline 0x77 24 x 
    Push25 x      -> inline 0x78 25 x 
    Push26 x      -> inline 0x79 26 x 
    Push27 x      -> inline 0x7a 27 x 
    Push28 x      -> inline 0x7b 28 x 
    Push29 x      -> inline 0x7c 29 x 
    Push30 x      -> inline 0x7d 30 x 
    Push31 x      -> inline 0x7e 31 x 
    Push32 x      -> inline 0x7f 32 x 
    Dup1          -> single 0x80 
    Dup2          -> single 0x81 
    Dup3          -> single 0x82 
    Dup4          -> single 0x83 
    Dup5          -> single 0x84 
    Dup6          -> single 0x85 
    Dup7          -> single 0x86 
    Dup8          -> single 0x87 
    Dup9          -> single 0x88 
    Dup10         -> single 0x89 
    Dup11         -> single 0x8a 
    Dup12         -> single 0x8b 
    Dup13         -> single 0x8c 
    Dup14         -> single 0x8d 
    Dup15         -> single 0x8e 
    Dup16         -> single 0x8f 
    Swap1         -> single 0x90 
    Swap2         -> single 0x91 
    Swap3         -> single 0x92 
    Swap4         -> single 0x93 
    Swap5         -> single 0x94 
    Swap6         -> single 0x95 
    Swap7         -> single 0x96 
    Swap8         -> single 0x97 
    Swap9         -> single 0x98 
    Swap10        -> single 0x99 
    Swap11        -> single 0x9a 
    Swap12        -> single 0x9b 
    Swap13        -> single 0x9c 
    Swap14        -> single 0x9d 
    Swap15        -> single 0x9e 
    Swap16        -> single 0x9f 
    Log0          -> single 0xa0 
    Log1          -> single 0xa1 
    Log2          -> single 0xa2 
    Log3          -> single 0xa3 
    Log4          -> single 0xa4 
    Create        -> single 0xf0 
    Call          -> single 0xf1 
    CallCode      -> single 0xf2 
    Return        -> single 0xf3 
    DelegateCall  -> single 0xf4 
    Suicide       -> single 0xf5

    Label x -> do
      p <- stPos <$> getState
      updateState $ \st -> st { stLabels = Map.insert x p (stLabels st) }
      single 0x5b -- i.e. JumpDest

    PushLabel x -> do
      refs <- stRefs <$> getState
      p    <- stPos  <$> getState 
      let refs' = (p + 1, x) : refs    -- The pointer will start at the *next* byte
      let p'    = p + 5                -- Write 5 bytes (push + 32-bit pointer)
      updateState $ \st -> 
        st { stRefs = refs'
           , stPos  = p' }
      return $ B.pack [0x63, 0, 0, 0, 0] -- i.e. Push4 then null ptr
      

-- Opcode constructors
-- ---------------------------------------------------------------------

swap :: Int -> Maybe Op
swap x = case x of
  1  -> Just Swap1
  2  -> Just Swap2
  3  -> Just Swap3
  4  -> Just Swap4
  5  -> Just Swap5
  6  -> Just Swap6
  7  -> Just Swap7
  8  -> Just Swap8
  9  -> Just Swap9
  10 -> Just Swap10
  11 -> Just Swap11
  12 -> Just Swap12
  13 -> Just Swap13
  14 -> Just Swap14
  15 -> Just Swap15
  16 -> Just Swap16
  _  -> Nothing

dup :: Int -> Maybe Op
dup x = case x of
  1  -> Just Dup1
  2  -> Just Dup2
  3  -> Just Dup3
  4  -> Just Dup4
  5  -> Just Dup5
  6  -> Just Dup6
  7  -> Just Dup7
  8  -> Just Dup8
  9  -> Just Dup9
  10 -> Just Dup10
  11 -> Just Dup11
  12 -> Just Dup12
  13 -> Just Dup13
  14 -> Just Dup14
  15 -> Just Dup15
  16 -> Just Dup16
  _  -> Nothing

-- | Push the literal value 0
push0 :: Op
push0 = Push1 (B.singleton 0)

-- | Push the literal value 32
push32 :: Op
push32 = Push1 (B.singleton 32)

pushInteger :: Integer -> Maybe Op
pushInteger x =
  let bs = unroll x
  in Just $ Push32 bs

pushInt :: Int -> Maybe Op
pushInt 0 = Just push0
pushInt x =
  let bs = unroll x
  in case B.length bs of
    1  -> Just $ Push1  bs
    2  -> Just $ Push2  bs
    3  -> Just $ Push3  bs
    4  -> Just $ Push4  bs
    5  -> Just $ Push5  bs
    6  -> Just $ Push6  bs
    7  -> Just $ Push7  bs
    8  -> Just $ Push8  bs
    9  -> Just $ Push9  bs
    10 -> Just $ Push10 bs
    11 -> Just $ Push11 bs
    12 -> Just $ Push12 bs
    13 -> Just $ Push13 bs
    14 -> Just $ Push14 bs
    15 -> Just $ Push15 bs
    16 -> Just $ Push16 bs
    17 -> Just $ Push17 bs
    18 -> Just $ Push18 bs
    19 -> Just $ Push19 bs
    20 -> Just $ Push20 bs
    21 -> Just $ Push21 bs
    22 -> Just $ Push22 bs
    23 -> Just $ Push23 bs
    24 -> Just $ Push24 bs
    25 -> Just $ Push25 bs
    26 -> Just $ Push26 bs
    27 -> Just $ Push27 bs
    28 -> Just $ Push28 bs
    29 -> Just $ Push29 bs
    30 -> Just $ Push30 bs
    31 -> Just $ Push31 bs
    32 -> Just $ Push32 bs
    _  -> Nothing


