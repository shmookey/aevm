module Fluidity.EVM.REPL.Command where

import Data.ByteString (ByteString)
import Data.List (isPrefixOf)
import System.Console.Haskeline.Completion

import Fluidity.EVM.Analyse.Pathfinder (PathPattern)
import Fluidity.EVM.Data.Value (Value)
import Fluidity.EVM.Data.ByteField (ByteField)
import Fluidity.EVM.Core.Interrupt (IntType, Action)
import Fluidity.EVM.Core.System (Strategy)


data Command
  = Chain    Chain
  | EVM      EVM
  | Meta     Meta
  | Walk     Walk
  | Monitor  Monitor
  | Parallel Parallel
  | State    State


-- Commands under EVM
-- ---------------------------------------------------------------------

data EVM
  = Go
  | Step
  | BreakAt Integer
  | Call Address Value Value ByteField
  | Enter Address Value Value ByteField
  | Show EVMShow
  | Interrupt Interrupt
  | Paths
  | Abort
  | Breakpoint Breakpoint

data CallData
  = RawCall ByteString
  | MethodCall String [MethodArg]

data MethodArg
  = NumArg Integer String
  | BoolArg Bool
  | AddrArg ByteString

data Breakpoint
  = BreakpointShow
  | BreakpointOn Int
  | BreakpointOff Int
  | BreakpointClear

data EVMShow
  = ShowStack
  | ShowMemory
  | ShowStorage
  | ShowCall
  | ShowCode (Maybe Slice)

data Interrupt
  = InterruptEcho [IntType]
  | InterruptBreak [IntType]
  | InterruptOff [IntType]
  | InterruptShow
  | InterruptAction Action
  | InterruptStrategy Strategy


-- Chain
-- ---------------------------------------------------------------------

data Chain
  = ChainBlock Block
  | ChainAccount Account

data Block
  = BlockCommit Integer
  | BlockShow (Maybe Integer)
  | BlockList

data Account
  = AccountList (Maybe Address)
  | AccountShow Address
  | AccountDrop Address
  | AccountCodeDisassemble Address
  | AccountCodeHexDump Address
  | AccountBalanceGet Address
  | AccountBalanceSet Address Integer
  | AccountStorageGet Address
  | AccountStorageGetKey Address ByteString
  | AccountStorageSetKey Address ByteString ByteString

data Walk
  = WalkMatch Address
  | WalkAddress Address Value

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
  = ParCall SetRef Value Value ByteField (Maybe PostProcess)
  | ParSets ParSets

data ParSets
  = ParSetList
  | ParSetShow String
  | ParSetDrop String
  | ParSetShowStorage String


-- Monitor
-- ---------------------------------------------------------------------

data Monitor
  = MonOn
  | MonOff


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

type Slice = (Maybe Int, Maybe Int)

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


-- Tab completeion
-- ---------------------------------------------------------------------
data Trace = TraceD

complete :: (String, String) -> IO (String, [Completion])
complete (revline, partial) =
  let
    line = reverse revline

    menu :: (String, [String]) -> [Completion]
    menu (name, items) =
      let
        prefix = case name of
          ""  -> ""
          _   -> name ++ " "
        elems = (++) <$> [prefix] <*> items
      in 
        map (\c -> Completion c (last $ words c) True) (filter (isPrefixOf line) elems)

    topLevel       = ("",                       ["chain", "evm", "mon", "par", "state", "walk", ":help", ":quit"])
    evmMenu        = ("evm",                    ["abort", "breakat", "breakpoint", "call", "enter", "go", "interrupt", "paths", "show", "step"])
    breakpointMenu = ("evm breakpoint",         ["clear", "off", "on", "show"])
    evmShowMenu    = ("evm show",               ["call", "code", "memory", "stack", "storage"])
    interruptMenu  = ("evm interrupt",          ["break", "echo", "off", "strategy", "show"])
    intBreakMenu   = ("evm interrupt break",    interrupts)
    intEchoMenu    = ("evm interrupt echo",     interrupts)
    intOffMenu     = ("evm interrupt off",      interrupts)
    intStratMenu   = ("evm interrupt strategy", ["immediate", "preempt", "wait"])
    chainMenu      = ("chain",                  ["account", "block"])
    blockMenu      = ("chain block",            ["commit", "list", "show"])
    accountMenu    = ("chain account",          ["balance", "code", "drop", "list", "show", "storage"])
    codeMenu       = ("chain account code",     ["disassemble", "hexdump"])
    balanceMenu    = ("chain account balance",  ["get", "set"])
    storageMenu    = ("chain account storage",  ["get", "getkey", "setkey"])
    parMenu        = ("par",                    ["call", "set"])
    parsetMenu     = ("par set",                ["drop", "list", "show", "showstorage"])
    monitorMenu    = ("mon",                    ["on", "off"])
    stateMenu      = ("state",                  ["drop", "list","load","save"])
    walkMenu       = ("walk",                   ["match", lblAddr])

    interrupts     = [ "call",  "cycle",  "emit",  "jump",   "jumpi"
                     , "ready", "return", "sload", "sstore", "stop" ]

    lblAddr        = "(address or prefix)"

  in do
    return . (,) partial $ case words line of
      "chain" :r -> case r of "block"      :r -> menu blockMenu
                              "account"    :r -> case r of "balance"  :r -> menu balanceMenu
                                                           "code"     :r -> menu codeMenu
                                                           "storage"  :r -> menu storageMenu
                                                           _             -> menu accountMenu
                              _               -> menu chainMenu
      "evm"   :r -> case r of "breakpoint" :r -> menu breakpointMenu
                              "show"       :r -> menu evmShowMenu
                              "interrupt"  :r -> case r of "break"    :r -> menu intBreakMenu
                                                           "echo"     :r -> menu intEchoMenu
                                                           "off"      :r -> menu intOffMenu
                                                           "strategy" :r -> menu intStratMenu
                                                           _             -> menu interruptMenu
                              _               -> menu evmMenu
      "mon"   :r -> case r of _               -> menu monitorMenu
      "par"   :r -> case r of "set"        :r -> menu parsetMenu
                              _               -> menu parMenu
      "state" :r -> case r of _               -> menu stateMenu
      "walk"  :r -> case r of _               -> menu walkMenu
      _          -> menu topLevel

