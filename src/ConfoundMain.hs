module Main where

import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Data.Text (pack, unpack)
import qualified Options.Applicative as Opts
import qualified Data.Text as T

import Control.Monad.Result
import Control.Monad.Resultant
import Confound.Types
import Confound.Contracts (load, enumerateTargets)
import Confound.Methods (createMethodList, identifyMethods)
import Confound.Util


listOption r = splitOn "," <$> Opts.strOption r
switchOff r = not <$> Opts.switch r

readCliOpts :: IO Config
readCliOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "confound -- evm bytecode analysis" 
   <> Opts.progDesc "Static and runtime analysis tools for the Ethereum Virtual Machine" )
  where 
    cliOpts = Config
      <$> Opts.strOption
          ( Opts.long    "dir"
         <> Opts.short   'd'
         <> Opts.value   "."
         <> Opts.help    "Snapshot directory" )
      <*> Opts.option Opts.auto
          ( Opts.long    "verbosity"
         <> Opts.short   'v'
         <> Opts.value   3
         <> Opts.help    "Verbosity level 0-5. Default: 3" )
      <*> listOption
          ( Opts.long    "method-categories"
         <> Opts.value   "fin,trade,bet"
         <> Opts.help    "Valid categories: fin,trade,bet,org,crud,sys. Default: fin,trade,bet" )
      <*> listOption
          ( Opts.long    "argtype-categories"
         <> Opts.value   "basic"
         <> Opts.help    "Valid categories: minimal,basic,extended,broad,all. Default: basic" )
      <*> Opts.option Opts.auto
          ( Opts.long    "arg-limit"
         <> Opts.value   3
         <> Opts.help    "Max args to search for. Default: 3" )
      <*> switchOff
          ( Opts.long    "no-initial-caps"
         <> Opts.help    "Look for lowercase method names only")
      <*> Opts.switch
          ( Opts.long    "verify-methods"
         <> Opts.help    "Check for evidence that a method match leads to a method. Default: false" )
      <*> Opts.many (Opts.argument Opts.str
          ( Opts.metavar "[HASH_PREFIXES...]"
         <> Opts.help    "Hash prefixes of contracts to analyse, omit to search all" ))

app :: App ()
app = do
  targetPrefixes <- getTargetPrefixes
  targets <- enumerateTargets
  methods <- createMethodList
  mapM_ (\x -> load x >>= identifyMethods methods) targets 
  return ()

main :: IO ()
main = do
  conf   <- readCliOpts
  (_, r) <- runResultantT app (State conf)
  case r of
    Ok xs -> return ()
    Err e -> putStrLn $ "Error: " ++ e


