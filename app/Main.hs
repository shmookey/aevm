import Prelude hiding (putStr, putStrLn)
import qualified Options.Applicative as Opts
import Data.Text (pack)
import Data.Text.IO (putStr, putStrLn)
import Data.Semigroup ((<>))
import qualified Data.Maybe as Maybe

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (typeset, withLineNumbers)

import Fluidity.EVM
import qualified Fluidity.EVM.REPL.Monad as R

data Options = Options
  { optDataDir  :: String
  , optCommand  :: Maybe Command
  }

data Command
  = REPL   REPLOpts
  | Import ImportOpts

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
      <*> Opts.optional (Opts.subparser $
            Opts.command "repl" (Opts.info replOpts
              ( Opts.progDesc "Start in interactive mode." ))
         <> Opts.command "import" (Opts.info importOpts
              ( Opts.progDesc "Import a Parity snapshot." )))
    importOpts = Import <$> (ImportOpts
      <$> Opts.argument Opts.str
          ( Opts.metavar "NAME"
         <> Opts.value "default"
         <> Opts.help "Name to assign this snapshot" )
      <*> Opts.argument Opts.str
          ( Opts.metavar "DIR"
         <> Opts.help "Parity snapshot directory" ))
    replOpts = REPL <$> (REPLOpts
      <$> Opts.optional (Opts.option Opts.auto
          ( Opts.long "max-chunks"
         <> Opts.short 'm'
         <> Opts.metavar "NUM"
         <> Opts.help "Maximum number of snapshot chunks to load" ))
      <*> Opts.switch
          ( Opts.long "no-dupes"
         <> Opts.short 'u'
         <> Opts.help "Unique contracts only, don't load duplicates.")
      <*> Opts.argument Opts.str
          ( Opts.metavar "NAME"
         <> Opts.value "default"
         <> Opts.help "Name of snapshot to load" ))

printError :: Error -> IO ()
printError x = do
  putStr (pack "Failed with error: ")
  putStrLn $ typeset x

runEVM :: FilePath -> EVM a -> IO (Result Error a)
runEVM dataDir ma = snd <$> runResultantT ma st
  where st = State dataDir

defaultCommand :: Command
defaultCommand = REPL $ REPLOpts Nothing False "default"

main :: IO ()
main = do
  opts   <- readCLIOpts
  result <- runEVM (optDataDir opts) $ 
    case Maybe.fromMaybe defaultCommand (optCommand opts) of
      REPL x   -> doREPL x
      Import x -> doImport x

  case result of
     Ok _                   -> return ()
     Err (REPLError R.Quit) -> putStrLn (pack "Quit")
     Err e                  -> printError e


