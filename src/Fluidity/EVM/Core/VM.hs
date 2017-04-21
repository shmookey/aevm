-- | Core VM module for the Fluidity EVM.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Core.VM where

import Prelude hiding (LT, GT, Value, fail, div, mod, exp, and, or)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Function (on)
import Data.Map (Map)
import Data.ByteString (ByteString)
import Control.Monad (when, void)
import Control.Monad.Loops (whileM_)
import qualified Data.Map as Map
import qualified Data.ByteString as B

import Control.Monad.Combinator
import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Execution hiding (interrupt)
import qualified Control.Monad.Execution as Exec

import Fluidity.Common.Binary (roll, toBytes)
import Fluidity.EVM.Data.Bytecode (Op(..))
import Fluidity.EVM.Types hiding (Op(..))
import Fluidity.EVM.Core.Blockchain (Blockchain, storageAt, setStorageAt)
import Fluidity.EVM.Data.ByteField hiding (size)
import Fluidity.EVM.Data.Operations
import Fluidity.EVM.Data.Prov (Prov(Sys, Nul), Sys(GasLeft))
import Fluidity.EVM.Data.Transaction
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Core.Interrupt (Interrupt, IntConf)
import qualified Fluidity.EVM.Data.ByteField as BF
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.Core.Blockchain as Chain
import qualified Fluidity.EVM.Core.Interrupt as INT


-- Public exports
-- ---------------------------------------------------------------------

type VM = Execution Blockchain Interrupt State Error

data State = State
  { stStatus  :: Status
  , stCall    :: MessageCall
  , stPC      :: Int
  , stCode    :: Bytecode
  , stStack   :: Stack
  , stMemory  :: Memory
  , stGas     :: Integer
  , stIntConf :: IntConf
  } deriving (Show, Generic, NFData)

data Error
  = StackUnderflow
  | InvalidJump         Int
  | InvalidPC           Int
  | NotImplemented      Op
  | BytecodeError       Bytecode.Error
  | UnexpectedNegative  Int
  | NotResumable
  | NoMessageCall
  | BadStatus
  | OutOfGas
  | EmptyCode
  deriving (Show, Generic, NFData)

data Status
  = Idle
  | Running
  | Halt
  deriving (Eq, Show, Generic, NFData)


-- | Starts the VM processing a message call
call :: VM ()
call = do
  assertStatus Idle

  addr       <- bytes <$> callee
  callGas   >>= setGas . uint
  callValue >>= env . Chain.credit addr
  code       <- env $ Chain.code addr

  when (BF.null code) $ 
    fail EmptyCode

  setCode code
  setStatus Running
  interrupt INT.Ready
  whileM_ isRunning step
  assertStatus Halt

-- | Resume running a VM
resume :: VM ()
resume = do
  assertStatus Running
  whileM_ isRunning step


-- Execution cycle
-- ---------------------------------------------------------------------

-- | Process the current instruction and increment the program counter
step :: VM ()
step = do
  (sz, op) <- nextOp
  chargeGas
  perform op
  updatePC (+sz)
  getPC  >>= interrupt . INT.Cycle

-- | Is the VM in a running state?
isRunning :: VM Bool
isRunning = withDefault False (assertStatus Running >> return True)

-- | Fail unless the VM is in a given running state
assertStatus :: Status -> VM ()
assertStatus x = getStatus >>= \s -> when (s /= x) $ fail BadStatus

-- | Peek at the next operation
nextOp :: VM (Int, Op)
nextOp = getPC >>= opAt

opAt :: Int -> VM (Int, Op)
opAt p = getCode >>= point . mapError BytecodeError . Bytecode.getOp p

chargeGas :: VM ()
chargeGas = do
  updateGas $ \x -> x - 1
  gas <- getGas
  if gas <= 0
  then fail OutOfGas
  else return ()

interrupt :: Interrupt -> VM ()
interrupt x = do
  ints <- getIntConf
  when (INT.interruptible x ints) $ Exec.interrupt x


-- Instruction actions
-- ---------------------------------------------------------------------

perform :: Op -> VM ()
perform op = case op of
  -- Stop and arithmetic operations
  Stop         -> setStatus Halt >> interrupt INT.Stop
  Add          -> pop2 >>= \(a, b) -> push $ a `add` b
  Mul          -> pop2 >>= \(a, b) -> push $ a `mul` b
  Div          -> pop2 >>= \(a, b) -> push $ a `div` b
  Sub          -> pop2 >>= \(a, b) -> push $ a `sub` b
  Mod          -> pop2 >>= \(a, b) -> push $ a `mod` b
  Exp          -> pop2 >>= \(a, b) -> push $ a `exp` b
  -- Comparison and bitwise logic operations
  LT           -> pop2 >>= \(a, b) -> push $ a `lt`   b
  GT           -> pop2 >>= \(a, b) -> push $ a `gt`   b
  SLT          -> pop2 >>= \(a, b) -> push $ a `slt`  b
  SGT          -> pop2 >>= \(a, b) -> push $ a `sgt`  b
  Eq           -> pop2 >>= \(a, b) -> push $ a `eq`   b
  And          -> pop2 >>= \(a, b) -> push $ a `and`  b
  Or           -> pop2 >>= \(a, b) -> push $ a `or`   b
  Xor          -> pop2 >>= \(a, b) -> push $ a `xor`  b
  Byte         -> pop2 >>= \(a, b) -> push $ a `byte` b
  IsZero       -> pop  >>= push . iszero
  Not          -> pop  >>= push . bitnot
  -- SHA3
  SHA3         -> pop2 >>= \(p, n) -> getMemory >>= push . sha3 p n
  -- Environmental information
  Balance      -> popb >>= env . Chain.balance >>= push
  CallDataSize -> callData >>= push . size
  CallDataLoad -> popi >>= \p -> callData >>= push . getWord p
  CallDataCopy -> pop3i >>= \(p, q, n) -> callData >>= \d -> getMemory >>= setMemory . copy q p n d
  CodeCopy     -> pop3i >>= \(p, q, n) -> getCode >>= \d -> getMemory >>= setMemory . copy q p n d
  ExtCodeSize  -> popb >>= env . Chain.code >>= push . size
  Caller       -> caller    >>= push
  CallValue    -> callValue >>= push
  Origin       -> caller    >>= push
  Address      -> callee    >>= push
  -- Block information
  BlockHash    -> env Chain.blockHash       >>= push
  Number       -> env Chain.blockNumber     >>= push
  Timestamp    -> env Chain.blockTime       >>= push
  GasPrice     -> env Chain.blockGasPrice   >>= push
  Difficulty   -> env Chain.blockDifficulty >>= push
  Coinbase     -> env Chain.blockCoinbase   >>= push
  -- Stack, memory, storage and flow operations
  Pop          -> void pop
  Jump         -> jump
  JumpI        -> jumpi
  JumpDest     -> return ()
  MLoad        -> popi >>= \p -> getMemory >>= push . getWord p
  MStore       -> pop2 >>= \(p, x) -> getMemory >>= setMemory . setWord (int p) x
  MStore8      -> pop2 >>= \(p, x) -> getMemory >>= setMemory . setByte (int p) x
  MSize        -> getMemory >>= push . size
  SLoad        -> (callee, pop) `to` \c i -> env (storageAt c i) >>= both_ push (interrupt . INT.SLoad i)
  SStore       -> (callee, pop2) `to` \c (k, v) -> env (setStorageAt c k v) >> interrupt (INT.SStore k v)
  Gas          -> getGas >>= push . valueFrom (Sys GasLeft)
  -- Push operations
  Push1  x     -> push x
  Push2  x     -> push x
  Push3  x     -> push x
  Push4  x     -> push x
  Push5  x     -> push x
  Push6  x     -> push x
  Push7  x     -> push x
  Push8  x     -> push x
  Push9  x     -> push x
  Push10 x     -> push x
  Push11 x     -> push x
  Push12 x     -> push x
  Push13 x     -> push x
  Push14 x     -> push x
  Push15 x     -> push x
  Push16 x     -> push x
  Push17 x     -> push x
  Push18 x     -> push x
  Push19 x     -> push x
  Push20 x     -> push x
  Push21 x     -> push x
  Push22 x     -> push x
  Push23 x     -> push x
  Push24 x     -> push x
  Push25 x     -> push x
  Push26 x     -> push x
  Push27 x     -> push x
  Push28 x     -> push x
  Push29 x     -> push x
  Push30 x     -> push x
  Push31 x     -> push x
  Push32 x     -> push x
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
  Log0         -> pop2i >>= \(p,n) -> getMemory >>= \m -> interrupt $ INT.Emit (fullslice p n m) []
  Log1         -> pop2i >>= \(p,n) -> getMemory >>= \m -> consume 1 >>= interrupt . INT.Emit (fullslice p n m)
  Log2         -> pop2i >>= \(p,n) -> getMemory >>= \m -> consume 2 >>= interrupt . INT.Emit (fullslice p n m)
  Log3         -> pop2i >>= \(p,n) -> getMemory >>= \m -> consume 3 >>= interrupt . INT.Emit (fullslice p n m)
  Log4         -> pop2i >>= \(p,n) -> getMemory >>= \m -> consume 4 >>= interrupt . INT.Emit (fullslice p n m)
  -- System operations
  Call         -> externalCall
  Return       -> pop2i >>= \x -> getMemory >>= interrupt . INT.Return . uncurry fullslice x >> setStatus Halt
  _            -> fail $ NotImplemented op

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
  ptr <- pop
  let p = int ptr
  op  <- snd <$> opAt p
  case op of
    JumpDest -> do interrupt $ INT.Jump ptr
                   setPC p
    _        -> fail $ InvalidJump p

-- | Implements the JUMPI instruction
jumpi :: VM ()
jumpi = do
  ptr  <- pop
  cond <- pop
  let p = int ptr
      c = int cond
  op   <- snd <$> opAt p
  interrupt $ INT.JumpI cond ptr
  when (c /= 0) $ case op of
    JumpDest -> setPC p
    _        -> fail $ InvalidJump p

-- | Send funds or messsage call into another contract
externalCall :: VM ()
externalCall = do
  (gas, to, val) <- pop3
  msgdata        <- pop2i >>= \x -> uncurry fullslice x <$> getMemory
  (retp, retsz)  <- pop2
  updateGas $ \x -> x - uint gas
  callee >>= env . flip Chain.debit val . toBytes
  interrupt INT.Call
    { INT.cGas    = gas
    , INT.cCallee = to
    , INT.cValue  = val
    , INT.cData   = msgdata
    , INT.cRetP   = retp
    , INT.cRetSz  = retsz
    }
  push $ value 1 Nul


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

pop    = consume 1 >>= return . (\[x]       -> x        )
pop2   = consume 2 >>= return . (\[x, y]    -> (x, y)   )
pop3   = consume 3 >>= return . (\[x, y, z] -> (x, y, z))
popi   = consume 1 >>= return . (\[x]       -> x        ) . map int
pop2i  = consume 2 >>= return . (\[x, y]    -> (x, y)   ) . map int
pop3i  = consume 3 >>= return . (\[x, y, z] -> (x, y, z)) . map int
popb   = consume 1 >>= return . (\[x]       -> x        ) . map toBytes
pop2b  = consume 2 >>= return . (\[x, y]    -> (x, y)   ) . map toBytes
pop3b  = consume 3 >>= return . (\[x, y, z] -> (x, y, z)) . map toBytes
push x = updateStack (x:)

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
initState :: MessageCall -> State
initState msg = State
  { stStatus  = Idle
  , stPC      = 0
  , stStack   = mempty
  , stMemory  = mempty
  , stGas     = 0
  , stCall    = msg
  , stCode    = mempty
  , stIntConf = INT.defaults
  }

getPC      = stPC      <$> getState
getStack   = stStack   <$> getState
getMemory  = stMemory  <$> getState
getStatus  = stStatus  <$> getState
getGas     = stGas     <$> getState
getCall    = stCall    <$> getState
getCode    = stCode    <$> getState
getIntConf = stIntConf <$> getState

caller     = msgCaller <$> getCall
callee     = msgCallee <$> getCall
callValue  = msgValue  <$> getCall
callGas    = msgGas    <$> getCall
callData   = msgData   <$> getCall

setPC      x = updateState (\st -> st { stPC      = x }) :: VM ()
setStack   x = updateState (\st -> st { stStack   = x }) :: VM ()
setMemory  x = updateState (\st -> st { stMemory  = x }) :: VM ()
setStatus  x = updateState (\st -> st { stStatus  = x }) :: VM ()
setGas     x = updateState (\st -> st { stGas     = x }) :: VM ()
setCode    x = updateState (\st -> st { stCode    = x }) :: VM ()
setIntConf x = updateState (\st -> st { stIntConf = x }) :: VM ()

updatePC      f = getPC      >>= setPC      . f
updateStack   f = getStack   >>= setStack   . f
updateMemory  f = getMemory  >>= setMemory  . f
updateStatus  f = getStatus  >>= setStatus  . f
updateGas     f = getGas     >>= setGas     . f
updateIntConf f = getIntConf >>= setIntConf . f

