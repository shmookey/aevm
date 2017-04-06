{-# LANGUAGE OverloadedStrings #-}

module Confound.Contracts where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import Data.List (any, isPrefixOf)
import qualified Data.Text as T
import System.FilePath ((</>), (<.>))

import Control.Monad.Resultant
import Confound.Types
import Confound.Util


load :: ContractID -> App Contract
load cid = do
  bytecode <- getBytecode cid
  copies   <- getCopies cid
  disasm   <- getDisassembly cid
  return $ Contract cid bytecode copies disasm

getBytecode :: ContractID -> App Bytecode
getBytecode cid = do
  path <- pathTo cid "bin"
  code <- lift $ readFile path
  return code

getCopies :: ContractID -> App [(ContractID, Balance)]
getCopies cid = 
  let
    readPair :: String -> App (ContractID, Balance)
    readPair xs = case words xs of
      (k:v:[]) -> return (k, read v)
      _        -> fail $ "Invalid line in balances file: " ++ xs
  in do
    contents <- pathTo cid "instances" >>= (lift . readFile)
    mapM readPair $ lines contents

getDisassembly :: ContractID -> App ASM
getDisassembly cid = do
  disasm <- pathTo cid "disasm" >>= (lift . readFile)
  return $ lines disasm

totalValue :: ContractID -> App Balance
totalValue cid = do
  copies <- getCopies cid
  return $ foldl (\a (_,v) -> a + v) 0 copies

pathTo :: ContractID -> String -> App FilePath
pathTo cid ext = do
  baseDir <- getBaseDir 
  return $ baseDir </> cid <.> ext

loadIndex :: App [ContractID]
loadIndex = do
  baseDir <- getBaseDir
  lines <$> (lift . readFile $ baseDir </> "INDEX")

enumerateTargets :: App [ContractID]
enumerateTargets = do
  index <- loadIndex
  prefixes <- getTargetPrefixes
  let targets = case prefixes of
                  [] -> index
                  _  -> filter (\x -> any (flip isPrefixOf x) prefixes) index
  logInfo $ "Found " ++ (show $ length targets) ++ " matching contracts"
  return targets


