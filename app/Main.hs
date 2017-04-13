import Prelude hiding (putStr, putStrLn)
import qualified Options.Applicative as Opts
import Data.Text (pack)
import Data.Text.IO (putStr, putStrLn)
import Data.Semigroup ((<>))

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (typeset, withLineNumbers)

import qualified Fluidity.EVM as EVM
import Fluidity.EVM (EVM, REPLOpts(..), Error)


data Options = Options
  { optBinaryInput :: Bool
  , optCommand     :: Command
  } deriving (Show)

data Command
  = Run FilePath
  | Disassemble FilePath
  | Import FilePath
  | REPL REPLOpts
  deriving (Show)

readCLIOpts :: IO Options
readCLIOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "flevm -- fluidity ethereum virtual machine"
   <> Opts.progDesc "Run and debug EVM programs" )
  where 
    cliOpts = Options
      <$> Opts.switch
          ( Opts.short 'b'
         <> Opts.long  "binary"
         <> Opts.help  "Specify binary-encoded input." )
      <*> Opts.subparser
          ( Opts.command "run" (Opts.info runOpts
            ( Opts.progDesc "Run an EVM program." ))
         <> Opts.command "repl" (Opts.info replOpts
            ( Opts.progDesc "Start in interactive mode." ))
         <> Opts.command "disasm" (Opts.info disasmOpts
            ( Opts.progDesc "Disassemble compiled EVM bytecode." ))
         <> Opts.command "import" (Opts.info importOpts
            ( Opts.progDesc "Import a Parity snapshot." )))
    runOpts = Run
      <$> Opts.argument Opts.str
          ( Opts.metavar "FILE"
         <> Opts.help "EVM program" )
    disasmOpts = Disassemble
      <$> Opts.argument Opts.str
          ( Opts.metavar "FILE"
         <> Opts.help "EVM program" )
    importOpts = Import
      <$> Opts.argument Opts.str
          ( Opts.metavar "DIR"
         <> Opts.help "Parity snapshot directory" )
    replOpts = REPL <$> (REPLOpts
      <$> Opts.strOption
          ( Opts.long "snapshot-dir"
         <> Opts.short 's'
         <> Opts.metavar "DIR"
         <> Opts.value ".aevm"
         <> Opts.help "Path to blockchain snapshot" )
      <*> Opts.optional (Opts.option Opts.auto
          ( Opts.long "max-chunks"
         <> Opts.short 'm'
         <> Opts.metavar "NUM"
         <> Opts.help "Maximum number of snapshot chunks to load" ))
      <*> Opts.switch
          ( Opts.long "no-dupes"
         <> Opts.short 'u'
         <> Opts.help "Unique contracts only, don't load duplicates."))

disasm :: Options -> FilePath -> EVM ()
disasm opts inputFile = error "not implemented"
  --prog <- EVM.load inputFile
  --EVM.printAsm prog

runProgram :: Options -> FilePath -> EVM ()
runProgram opts inputFile = error "not implemented"
  --prog <- EVM.load inputFile
  --EVM.runProgram prog

runImport :: FilePath -> EVM ()
runImport dir = EVM.importSnapshot dir ".aevm"

printError :: Error -> IO ()
printError x = do
  putStr (pack "Failed with error: ")
  putStrLn $ typeset x

main :: IO ()
main = do
  opts   <- readCLIOpts
  result <- EVM.run $ case optCommand opts of
    Run x         -> runProgram opts x
    Disassemble x -> disasm opts x
    REPL x        -> EVM.repl x
    Import x      -> runImport x
  case result of
     Ok _  -> return ()
     Err e -> printError e


