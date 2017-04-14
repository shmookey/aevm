import Prelude hiding (putStr, putStrLn)
import qualified Options.Applicative as Opts
import Data.Text (pack)
import Data.Text.IO (putStr, putStrLn)
import Data.Semigroup ((<>))

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (typeset, withLineNumbers)

import Fluidity.EVM


data Options = Options
  { optDataDir  :: String
  , optCommand  :: Maybe Command
  }

data Command
  = Import ImportOpts
  | REPL   REPLOpts

readCLIOpts :: IO Options
readCLIOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "flevm -- fluidity ethereum virtual machine"
   <> Opts.progDesc "Run and debug EVM programs" )
  where 
    cliOpts = Options
      <$> Opts.strOption
          ( Opts.long "data-dir"
         <> Opts.short 'd'
         <> Opts.metavar "DIR"
         <> Opts.value ".aevm"
         <> Opts.help "Use a custom location for aevm data" )
      <*> Opts.optional (Opts.subparser
         <> Opts.command "repl" (Opts.info replOpts
            ( Opts.progDesc "Start in interactive mode." ))
         <> Opts.command "import" (Opts.info importOpts
            ( Opts.progDesc "Import a Parity snapshot." )))
    importOpts = Import <$> (ImportOpts
      <$> Opts.argument Opts.str
          ( Opts.metavar "NAME"
         <> Opts.value "default"
         <> Opts.help "Name to assign this snapshot" ))
      <$> Opts.argument Opts.str
          ( Opts.metavar "DIR"
         <> Opts.help "Parity snapshot directory" )
    replOpts = REPL <$> (REPLOpts
      <$> Opts.optional (Opts.option Opts.auto
          ( Opts.long "max-chunks"
         <> Opts.short 'm'
         <> Opts.metavar "NUM"
         <> Opts.help "Maximum number of snapshot chunks to load" ))
      <*> Opts.switch
          ( Opts.long "no-dupes"
         <> Opts.short 'u'
         <> Opts.help "Unique contracts only, don't load duplicates."))
      <*> Opts.argument Opts.str
          ( Opts.metavar "NAME"
         <> Opts.value "default"
         <> Opts.help "Name of snapshot to load" )

runImport :: FilePath -> EVM ()
runImport dir = EVM.importSnapshot dir ".aevm"

printError :: Error -> IO ()
printError x = do
  putStr (pack "Failed with error: ")
  putStrLn $ typeset x

main :: IO ()
main = do
  opts <- readCLIOpts
  st   <- State optDataDir 
  result <- EVM.run $ case optCommand opts of
    REPL x        -> EVM.repl x
    Import x      -> EVM.importSnapshot x
  case result of
     Ok _  -> return ()
     Err e -> printError e


