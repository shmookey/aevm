-- | Core VM module for the Fluidity EVM.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.VM where

import Prelude hiding (LT, GT, Value, fail)
import qualified Data.Array as Array
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Control.Monad (when, void)
import Control.Monad.Loops (whileM_)
import GHC.Generics (Generic)
import Control.DeepSeq

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Execution

import Fluidity.EVM.Bytecode
import Fluidity.EVM.Types
import Fluidity.EVM.Blockchain (Blockchain, MessageCall(..))
import Fluidity.EVM.Data (Value, ByteField, toInt, asUInt,
   (/+/), (/*/), (/-/), (///), (/**/), (/&/), (/|/), (/^/), (/!!/), (/</),(/>/),(/==/))
import qualified Fluidity.EVM.Decode as Decode
import qualified Fluidity.Common.Binary as Bin
import qualified Fluidity.EVM.Data as Data
import qualified Fluidity.EVM.Blockchain as Blockchain


-- Public exports
-- ---------------------------------------------------------------------

type VM = Execution Blockchain Interrupt State Error

data State = State
  { stRunState    :: RunState
  , stCall        :: MessageCall
  , stPC          :: PC
  , stBytecode    :: Bytecode
  , stStack       :: Stack
  , stMemory      :: Memory
  , stLogs        :: [Log]
  , stGas         :: Gas
  , stReturnData  :: ByteField
  } deriving (Show, Generic, NFData)

data Interrupt 
  = BeginCycle Int
  | StorageRead Value Value
  | StorageWrite Value Value
  | LogEmit ByteField [Value]
  | CheckBalance Address Balance
  | ExternalCall ExtCall
  | ProgramLoad
  | Reset
  | Stopped
  | ConditionalJump
  | GenericInterrupt
  | ReturnData ByteField
  deriving (Show, Generic, NFData)

data Error
  = StackUnderflow
  | InvalidJump         CAddr
  | InvalidPC           PC
  | NotImplemented      Op
  | DecodeError         Decode.Error
  | UnexpectedNegative  Int
  | NotResumable
  | OutOfGas
  deriving (Show, Generic, NFData)

data RunState
  = Running
  | Halt
  | Error Error
  deriving (Show, Generic, NFData)


-- | Message call into a contract by address. Resets the VM.
call :: MessageCall -> VM ()
call x = do
  setState initState
  setCall x

  let addr  = msgCallee x
  let gas   = msgGas x
  let value = msgValue x

  bytecode <- env $ Blockchain.getCode addr
  setBytecode bytecode

  setGas $ gas
  env . Blockchain.updateBalance addr $ Data.add value

  interrupt ProgramLoad
  whileM_ isRunning step

-- | Resume running a VM
resume :: VM ()
resume = do
  runState <- getRunState
  case runState of
    Running -> return ()
    _       -> fail NotResumable

  whileM_ isRunning step


-- Execution cycle
-- ---------------------------------------------------------------------

-- | Process the current instruction and increment the program counter
step :: VM ()
step = 
  let
    incrementPC :: Integer -> VM ()    
    incrementPC n = updatePC . Data.add $ Data.mkInternal n

  in do
    getPC >>= interrupt . BeginCycle . toInt
    (op, skip) <- nextOp
    perform op
    incrementPC $ toInteger skip

-- | Is the VM in a runnable state?
isRunning :: VM Bool
isRunning = getRunState >>= \st -> case st of
  Running -> return True
  _       -> return False

-- | Peek at the next operation
nextOp :: VM (Op, Int)
nextOp = do
  bytecode <- getBytecode
  counter  <- getPC
  let pc = toInt counter
  when (pc >= B.length bytecode) $ fail (InvalidPC counter)
  point . mapError DecodeError $ Decode.decode1 pc bytecode

opAt :: Int -> VM (Op, Int)
opAt ptr = do
  bytecode <- getBytecode
  point . mapError DecodeError $ Decode.decode1 ptr bytecode

chargeGas :: VM ()
chargeGas = do
  updateGas (\g -> g /-/ Data.mkGasMeter 1)
  gas <- getGas
  if toInt gas <= 0
  then fail OutOfGas
  else return ()


-- Instruction actions
-- ---------------------------------------------------------------------

perform :: Op -> VM ()
perform op = do
  pc         <- getPC
  mem        <- getMemory
  caller     <- getCaller
  callee     <- getCallee
  calldata   <- getCallData
  callvalue  <- getCallValue
  gas        <- getGas
  chargeGas

  case op of
    CData _      -> fail $ InvalidPC pc
    -- Stop and arithmetic operations
    Stop         -> setRunState Halt >> interrupt Stopped
    Add          -> pop2 >>= \(a, b) -> push $ a /+/ b
    Mul          -> pop2 >>= \(a, b) -> push $ a /*/ b
    Div          -> pop2 >>= \(a, b) -> push $ a /// b
    Sub          -> pop2 >>= \(a, b) -> push $ a /-/ b
    Mod          -> pop2 >>= \(a, b) -> push $ a `Data.umod` b
    Exp          -> pop2 >>= \(a, b) -> push $ a /**/ b
    -- Comparison and bitwise logic operations
    LT           -> pop2 >>= \(a, b) -> push $ a /</ b
    GT           -> pop2 >>= \(a, b) -> push $ a /</ b
    SLT          -> pop2 >>= \(a, b) -> push $ a `Data.slt` b
    SGT          -> pop2 >>= \(a, b) -> push $ a `Data.sgt` b
    Eq           -> pop2 >>= \(a, b) -> push $ a /==/ b
    And          -> pop2 >>= \(a, b) -> push $ a /&/ b
    Or           -> pop2 >>= \(a, b) -> push $ a /|/ b
    Xor          -> pop2 >>= \(a, b) -> push $ a /^/ b
    Byte         -> pop2 >>= \(a, b) -> push $ a /!!/ b
    IsZero       -> pop  >>= push . Data.iszero
    Not          -> pop  >>= push . Data.bitwisenot
    -- SHA3
    SHA3         -> pop2 >>= push . uncurry (Data.sha3 mem)
    -- Environmental information
    Balance      -> doBalance
    CallDataSize -> push $ Data.calldatasize calldata
    CallDataLoad -> pop  >>= push . Data.calldataload calldata
    CallDataCopy -> pop3 >>= setMemory . uncurry3 (Data.calldatacopy calldata mem)
    CodeCopy     -> codecopy
    ExtCodeSize  -> pop >>= env . Blockchain.getCodeSize >>= push 
    Caller       -> push caller
    CallValue    -> push callvalue
    Origin       -> push caller
    Address      -> push callee
    -- Block information
    BlockHash    -> env Blockchain.currentBlockHash       >>= push
    Number       -> env Blockchain.currentBlockNumber     >>= push
    Timestamp    -> env Blockchain.currentBlockTime       >>= push
    GasPrice     -> env Blockchain.currentBlockGasPrice   >>= push
    Difficulty   -> env Blockchain.currentBlockDifficulty >>= push
    Coinbase     -> env Blockchain.currentBlockCoinbase   >>= push
    -- Stack, memory, storage and flow operations
    Pop          -> void pop
    Jump         -> jump
    JumpI        -> jumpi
    JumpDest     -> return ()
    MLoad        -> pop  >>= push . Data.mload mem
    MStore       -> pop2 >>= setMemory . uncurry (Data.mstore8 mem)
    MStore8      -> pop2 >>= setMemory . uncurry (Data.mstore8 mem)
    MSize        -> push $ Data.mkMSize (toInteger $ Data.size mem) -- inaccurate due to bytefield allocations
    SLoad        -> pop  >>= sload >>= push
    SStore       -> pop2 >>= uncurry sstore
    Gas          -> push gas
    -- Push operations
    Push1  x     -> push . Data.mkPush  1 $ Bin.roll x
    Push2  x     -> push . Data.mkPush  2 $ Bin.roll x
    Push3  x     -> push . Data.mkPush  3 $ Bin.roll x
    Push4  x     -> push . Data.mkPush  4 $ Bin.roll x
    Push5  x     -> push . Data.mkPush  5 $ Bin.roll x
    Push6  x     -> push . Data.mkPush  6 $ Bin.roll x
    Push7  x     -> push . Data.mkPush  7 $ Bin.roll x
    Push8  x     -> push . Data.mkPush  8 $ Bin.roll x
    Push9  x     -> push . Data.mkPush  9 $ Bin.roll x
    Push10 x     -> push . Data.mkPush 10 $ Bin.roll x
    Push11 x     -> push . Data.mkPush 11 $ Bin.roll x
    Push12 x     -> push . Data.mkPush 12 $ Bin.roll x
    Push13 x     -> push . Data.mkPush 13 $ Bin.roll x
    Push14 x     -> push . Data.mkPush 14 $ Bin.roll x
    Push15 x     -> push . Data.mkPush 15 $ Bin.roll x
    Push16 x     -> push . Data.mkPush 16 $ Bin.roll x
    Push17 x     -> push . Data.mkPush 17 $ Bin.roll x
    Push18 x     -> push . Data.mkPush 18 $ Bin.roll x
    Push19 x     -> push . Data.mkPush 19 $ Bin.roll x
    Push20 x     -> push . Data.mkPush 20 $ Bin.roll x
    Push21 x     -> push . Data.mkPush 21 $ Bin.roll x
    Push22 x     -> push . Data.mkPush 22 $ Bin.roll x
    Push23 x     -> push . Data.mkPush 23 $ Bin.roll x
    Push24 x     -> push . Data.mkPush 24 $ Bin.roll x
    Push25 x     -> push . Data.mkPush 25 $ Bin.roll x
    Push26 x     -> push . Data.mkPush 26 $ Bin.roll x
    Push27 x     -> push . Data.mkPush 27 $ Bin.roll x
    Push28 x     -> push . Data.mkPush 28 $ Bin.roll x
    Push29 x     -> push . Data.mkPush 29 $ Bin.roll x
    Push30 x     -> push . Data.mkPush 30 $ Bin.roll x
    Push31 x     -> push . Data.mkPush 31 $ Bin.roll x
    Push32 x     -> push . Data.mkPush 32 $ Bin.roll x
    -- Duplication operations
    Dup1         -> stackIndex 0  >>= push
    Dup2         -> stackIndex 1  >>= push
    Dup3         -> stackIndex 2  >>= push
    Dup4         -> stackIndex 3  >>= push
    Dup5         -> stackIndex 4  >>= push
    Dup6         -> stackIndex 5  >>= push
    Dup7         -> stackIndex 6  >>= push
    Dup8         -> stackIndex 7  >>= push
    Dup9         -> stackIndex 8  >>= push
    Dup10        -> stackIndex 9  >>= push
    Dup11        -> stackIndex 10 >>= push
    Dup12        -> stackIndex 11 >>= push
    Dup13        -> stackIndex 12 >>= push
    Dup14        -> stackIndex 13 >>= push
    Dup15        -> stackIndex 14 >>= push
    Dup16        -> stackIndex 15 >>= push
    -- Exchange operations
    Swap1        -> swap 1
    Swap2        -> swap 2
    Swap3        -> swap 3
    Swap4        -> swap 4
    Swap5        -> swap 5
    Swap6        -> swap 6
    Swap7        -> swap 7
    Swap8        -> swap 8
    Swap9        -> swap 9
    Swap10       -> swap 10
    Swap11       -> swap 11
    Swap12       -> swap 12
    Swap13       -> swap 13
    Swap14       -> swap 14
    Swap15       -> swap 15
    Swap16       -> swap 16
    -- Logging operations
    Log0         -> pop2 >>= \(p,n) -> addLog (Data.logcopy mem p n) []
    Log1         -> pop2 >>= \(p,n) -> consume 1 >>= addLog (Data.logcopy mem p n)
    Log2         -> pop2 >>= \(p,n) -> consume 2 >>= addLog (Data.logcopy mem p n)
    Log3         -> pop2 >>= \(p,n) -> consume 3 >>= addLog (Data.logcopy mem p n)
    Log4         -> pop2 >>= \(p,n) -> consume 4 >>= addLog (Data.logcopy mem p n)
    -- System operations
    Call         -> externalCall
    Return       -> doReturn
    _            -> fail $ NotImplemented op


-- | Push a word onto the stack
push :: Value -> VM ()
push x = updateStack (x:)

-- | Take and remove the first stack item
pop :: VM Value
pop = consume 1 >>= (\[x] -> return x)

-- | Swap the value at the head of the stack with the value at an index
swap :: Int -> VM ()
swap i = do
  x <- stackIndex 0
  y <- stackIndex i
  setStackIndex 0 y
  setStackIndex i x

-- | Implements the JUMP instruction
jump :: VM ()
jump = do
  ptr  <- pop
  code <- getBytecode
  let p = toInt ptr
  (op,_) <- opAt p
  if
    p >= B.length code
  then
    fail $ InvalidJump ptr
  else if
    p < 0
  then
    fail $ UnexpectedNegative p
  else case op of
    JumpDest -> setPC ptr
    _        -> fail $ InvalidJump ptr

-- | Implements the JUMPI instruction
jumpi :: VM ()
jumpi = do
  ptr  <- pop
  cond <- pop
  code <- getBytecode
  let p = toInt ptr
  (op,_) <- opAt p
  if
    toInt cond == 0
  then
    return ()
  else if
    p >= B.length code
  then
    fail $ InvalidJump ptr
  else if
    p < 0
  then
    fail $ UnexpectedNegative p
  else case op of
    JumpDest -> setPC ptr
    _        -> fail $ InvalidJump ptr

-- | Load a word from storage
sload :: SAddr -> VM Value
sload p = do
  storage <- getCallee >>= env . Blockchain.getStorage
  let v = Map.findWithDefault Data.mkNil p storage
  interrupt $ StorageRead p v
  return $ Data.mkSLoad p v

-- | Save a word to storage
sstore :: SAddr -> Value -> VM ()
sstore k new = do
  addr <- getCallee
  env . Blockchain.updateStorage addr $ Map.insert k (Data.mkSStore k new)
  interrupt $ StorageWrite k new

codecopy :: VM ()
codecopy = do
  mptr <- pop
  cptr <- pop
  sz   <- pop
  code <- (B.take (toInt sz) . B.drop (toInt cptr)) <$> getBytecode
  mem  <- getMemory
  let bf = Data.prefill Data.Code code
  let mem' = Data.calldatacopy bf mem mptr (Data.mkInternal 0) sz
  setMemory mem'

-- | Emit a log event
addLog :: ByteField -> [Value] -> VM ()
addLog bytes topics = do
  updateLogs $ (:) (Log bytes topics)
  interrupt $ LogEmit bytes topics

-- | Send funds or messsage call into another contract
externalCall :: VM ()
externalCall = do
  mem <- getMemory
  gas <- pop
  to  <- pop
  val <- pop
  msg <- uncurry (Data.callcopy mem) <$> pop2
  p   <- pop
  sz  <- pop
  updateGas (Data.sub gas)
  addr <- getCallee
  env $ Blockchain.updateBalance addr (Data.sub val)
  let ec = (gas, to, val, msg, p, sz)
  interrupt $ ExternalCall ec
  push (Data.mkInternal 1)

doReturn :: VM ()
doReturn = do
  (p,sz) <- pop2
  mem <- getMemory
  let bs = Data.callcopy mem p sz
  setReturnData bs
  interrupt $ ReturnData bs 
  setRunState Halt

getStorage :: VM Storage
getStorage = getCallee >>= env . Blockchain.getStorage

doBalance :: VM ()
doBalance = do
  addr    <- pop
  balance <- env $ Blockchain.getBalance addr
  interrupt $ CheckBalance addr balance
  push balance


-- Internal stack manipulation
-- ---------------------------------------------------------------------

-- | Take and remove some items from the stack
consume :: Int -> VM [Value]
consume n = do
  stack   <- getStack
  (xs,ys) <- if (length stack) >= n then return (splitAt n stack)
             else fail StackUnderflow
  setStack ys
  return xs

-- | Consume the first two stack items
pop2 :: VM (Value, Value)
pop2 = consume 2 >>= (\[x,y] -> return (x,y))

-- | Consume the first two stack items
pop3 :: VM (Value, Value, Value)
pop3 = consume 3 >>= (\[x,y,z] -> return (x,y,z))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- | Get the word at a 0-indexed position in the stack
stackIndex :: Int -> VM Value
stackIndex i = do
  stack <- getStack
  if
    (length stack) <= i 
  then
    fail StackUnderflow
  else if
    i < 0
  then
    fail $ UnexpectedNegative i
  else
    return $ stack !! i

-- | Set the word at a 0-indexed position in the stack
setStackIndex :: Int -> Value -> VM ()
setStackIndex i x = do
  stack     <- getStack
  (xs,_:ys) <- if (length stack) > i then return $ splitAt i stack
               else fail StackUnderflow
  setStack $ (xs ++ (x:ys))


-- VM Monad
-- ---------------------------------------------------------------------

-- | The default initial state of the VM
initState :: State
initState = State
  { stRunState   = Running
  , stPC         = Data.mkInternal 0
  , stStack      = mempty
  , stMemory     = mempty
  , stLogs       = mempty
  , stGas        = Data.mkInitialGas 0
  , stCall       = Blockchain.initMessageCall
  , stReturnData = mempty
  , stBytecode   = mempty
  }

getPC        = stPC       <$> getState
getStack     = stStack    <$> getState
getMemory    = stMemory   <$> getState
getRunState  = stRunState <$> getState
getLogs      = stLogs     <$> getState
getGas       = stGas      <$> getState
getCall      = stCall     <$> getState
getBytecode  = stBytecode <$> getState

getCaller    = msgCaller  <$> getCall
getCallee    = msgCallee  <$> getCall
getCallValue = msgValue   <$> getCall
getCallGas   = msgGas     <$> getCall
getCallData  = msgData    <$> getCall

setPC         x = updateState $ \st -> st { stPC         = x }
setStack      x = updateState $ \st -> st { stStack      = x }
setMemory     x = updateState $ \st -> st { stMemory     = x }
setCall       x = updateState $ \st -> st { stCall       = x }
setRunState   x = updateState $ \st -> st { stRunState   = x }
setLogs       x = updateState $ \st -> st { stLogs       = x }
setGas        x = updateState $ \st -> st { stGas        = x }
setReturnData x = updateState $ \st -> st { stReturnData = x }
setBytecode   x = updateState $ \st -> st { stBytecode   = x }

updatePC       f = getPC       >>= setPC       . f
updateStack    f = getStack    >>= setStack    . f
updateMemory   f = getMemory   >>= setMemory   . f
updateRunState f = getRunState >>= setRunState . f
updateLogs     f = getLogs     >>= setLogs     . f
updateGas      f = getGas      >>= setGas      . f

