module Fluidity.EVM.REPL.Command where

import Data.ByteString (ByteString)
import Data.List (isPrefixOf)
import System.Console.Haskeline.Completion

import Fluidity.EVM.Analyse.Pathfinder (PathPattern)
import Fluidity.EVM.Data.Value (Value)
import Fluidity.EVM.Data.ByteField (ByteField)
import Fluidity.EVM.Core.Interrupt (IntType)
import Fluidity.EVM.Core.System (InterruptAction, InterruptPoint)


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
  | Inspect Inspect
  | Interrupt Interrupt
  | Paths
  | Abort

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

data Interrupt
  = InterruptOn [IntType]
  | InterruptOff [IntType]
  | InterruptShow
  | InterruptAction InterruptAction
  | InterruptPoint InterruptPoint


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
  | AccountCodeDisassemble Address
  | AccountCodeHexDump Address
  | AccountBalanceGet Address
  | AccountBalanceSet Address Integer
  | AccountStorageGet Address
  | AccountStorageGetKey Address ByteString
  | AccountStorageSetKey Address ByteString ByteString

data Walk
  = WalkMatch Address

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

    topLevel      = ("",                      ["chain", "evm", "mon", "par", "state", "walk", ":help", ":quit"])
    evmMenu       = ("evm",                   ["abort", "breakat", "call", "go", "inspect", "interrupt", "paths", "step"])
    inspectMenu   = ("evm inspect",           ["call", "code", "memory", "stack", "storage"])
    interruptMenu = ("evm interrupt",         ["action", "off", "on", "point", "show"])
    intOnMenu     = ("evm interrupt on",      interrupts)
    intOffMenu    = ("evm interrupt off",     interrupts)
    intActMenu    = ("evm interrupt action",  ["break", "echo", "ignore"])
    intPointMenu  = ("evm interrupt point",   ["finalize", "immediate", "preempt"])
    chainMenu     = ("chain",                 ["account", "block"])
    blockMenu     = ("chain block",           ["commit", "list", "show"])
    accountMenu   = ("chain account",         ["balance", "code", "drop", "list", "show", "storage"])
    codeMenu      = ("chain account code",    ["disassemble", "hexdump"])
    balanceMenu   = ("chain account balance", ["get", "set"])
    storageMenu   = ("chain account storage", ["get", "getkey", "setkey"])
    parMenu       = ("par",                   ["call", "set"])
    parsetMenu    = ("par set",               ["drop", "list", "show", "showstorage"])
    monitorMenu   = ("mon",                   ["on", "off"])
    stateMenu     = ("state",                 ["drop", "list","load","save"])
    walkMenu      = ("walk",                  ["match"])

    interrupts    = [ "call",  "cycle",  "emit",  "jump",   "jumpi"
                    , "ready", "return", "sload", "sstore", "stop" ]

  in do
    return . (,) partial $ case words line of
      "chain" :r -> case r of "block"     :r -> menu blockMenu
                              "account"   :r -> case r of "balance" :r -> menu balanceMenu
                                                          "code"    :r -> menu codeMenu
                                                          "storage" :r -> menu storageMenu
                                                          _            -> menu accountMenu
                              _              -> menu chainMenu
      "evm"   :r -> case r of "inspect"   :r -> menu inspectMenu
                              "interrupt" :r -> case r of "off"     :r -> menu intOffMenu
                                                          "on"      :r -> menu intOnMenu
                                                          "action"  :r -> menu intActMenu
                                                          "point"   :r -> menu intPointMenu
                                                          _            -> menu interruptMenu
                              _              -> menu evmMenu
      "mon"   :r -> case r of _              -> menu monitorMenu
      "par"   :r -> case r of "set"       :r -> menu parsetMenu
                              _              -> menu parMenu
      "state" :r -> case r of _              -> menu stateMenu
      "walk"  :r -> case r of _              -> menu walkMenu
      _          -> menu topLevel

