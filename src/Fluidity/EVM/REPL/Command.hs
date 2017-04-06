module Fluidity.EVM.REPL.Command where

import Data.ByteString (ByteString)


data Command
  = EVM      EVM
  | State    State
  | Chain    Chain
  | Trace    Trace
  | Parallel Parallel
  | Meta     Meta


-- Commands under EVM
-- ---------------------------------------------------------------------

data EVM
  = Go
  | Step
  | BreakAt Integer
  | Call Address Integer (Maybe Integer) (Maybe CallData)
  | Inspect Inspect

data CallData
  = RawCall ByteString
  | MethodCall String [MethodArg]

data MethodArg
  = NumArg Integer String
  | BoolArg Bool
  | AddrArg ByteString

data Inspect
  = InspectStack
  | InspectMemory
  | InspectStorage
  | InspectCall
  | InspectCode (Maybe CodeRef)


-- Chain
-- ---------------------------------------------------------------------

data Chain
  = ChainBlock Block
  | ChainAccount Account

data Block
  = BlockCommit
  | BlockShow (Maybe Integer)
  | BlockList

data Account
  = AccountList (Maybe Address)
  | AccountShow Address
  | AccountDrop Address
  | AccountBalanceGet Address
  | AccountBalanceSet Address Integer
  | AccountStorageGet Address
  | AccountStorageGetKey Address ByteString
  | AccountStorageSetKey Address ByteString ByteString


-- State
-- ---------------------------------------------------------------------

data State
  = StateSave
  | StateLoad Integer
  | StateDrop Integer
  | StateList


-- Parallel
-- ---------------------------------------------------------------------

data Parallel
  = ParCall SetRef Integer (Maybe Integer) (Maybe CallData) (Maybe PostProcess)
  | ParSets ParSets

data ParSets
  = ParSetList
  | ParSetShow String
  | ParSetDrop String
  | ParSetShowStorage String


-- Meta
-- --------------------------------------------------------------------

data Meta
  = Help Help
  | Quit

data Help
  = HelpDefault
  | HelpTopic String


-- Common argument types
-- ---------------------------------------------------------------------

type CodeRef = (Maybe Address, Maybe Slice)

data Address
  = Address ByteString
  | Prefix ByteString

data SetRef
  = SetAlias String
  | SetRange Address

type Slice = (Maybe Integer, Maybe Integer)

type PostProcess = ([Filter], Maybe SaveTarget)

data Filter
  = FilterSStore
  | FilterSLoad
  | FilterThrow
  | FilterOk
  | FilterErr
  | FilterCall
  | FilterSend
  | FilterNotImpl

type SaveTarget = String


-- To be reworked or removed
-- ---------------------------------------------------------------------

data Trace
  = TraceValue Integer

