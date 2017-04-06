import Prelude hiding (putStr, putStrLn)
import qualified Options.Applicative as Opts
import Data.Text (pack)
import Data.Text.IO (putStr, putStrLn)
import Data.Semigroup ((<>))

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (typeset, withLineNumbers)

import qualified Fluidity.EVM as EVM
import Fluidity.EVM (EVM, Error)


data Options = Options
  { optBinaryInput :: Bool
  , optCommand     :: Command
  } deriving (Show)

data Command
  = Run FilePath
  | Disassemble FilePath
  | REPL (Maybe FilePath) FilePath
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
            ( Opts.progDesc "Disassemble compiled EVM bytecode." )))
    runOpts = Run
      <$> Opts.argument Opts.str
          ( Opts.metavar "FILE"
         <> Opts.help "EVM program" )
    disasmOpts = Disassemble
      <$> Opts.argument Opts.str
          ( Opts.metavar "FILE"
         <> Opts.help "EVM program" )
    replOpts = REPL
      <$> Opts.optional (Opts.strOption
          ( Opts.long "snapshot"
         <> Opts.short 's'
         <> Opts.metavar "DIR"
         <> Opts.help "Directory containing contracts" ))
      <*> Opts.argument Opts.str
          ( Opts.metavar "FILE"
         <> Opts.help "EVM program binary, or a list of filenames (snapshot mode)" )

disasm :: Options -> FilePath -> EVM ()
disasm opts inputFile = error "not implemented"
  --prog <- EVM.load inputFile
  --EVM.printAsm prog

runProgram :: Options -> FilePath -> EVM ()
runProgram opts inputFile = error "not implemented"
  --prog <- EVM.load inputFile
  --EVM.runProgram prog

repl :: Maybe FilePath -> FilePath -> EVM ()
repl dir inputFile = EVM.repl dir inputFile

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
    REPL m x      -> repl m x
  case result of
     Ok _  -> return ()
     Err e -> printError e


