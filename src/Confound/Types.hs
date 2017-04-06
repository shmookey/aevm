{-# LANGUAGE OverloadedStrings #-}

module Confound.Types where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)

import Control.Monad.Resultant


type Balance = Integer
type ContractID = String
type MethodID = String
type Error = String
type Bytecode = String
type Name = String
type ASM = [String]

type App = ResultantT IO State Error

data Match
  = MethodID Bytecode Name [Name]

data Contract = Contract
  { ctID          :: ContractID
  , ctBytecode    :: Bytecode
  , ctCopies      :: [(ContractID, Balance)]
  , ctDisassembly :: ASM
  }

data Config = Config
  { confDir               :: FilePath
  , confVerbosity         :: Int
  , confMethodCategories  :: [String]
  , confArgTypeCategories :: [String]
  , confArgLimit          :: Int
  , confInitialCaps       :: Bool
  , confVerifyMethods     :: Bool
  , confTargetPrefixes    :: [String]
  }

data State = State
  { stConfig  :: Config
  }


getConfig :: App Config
getConfig = stConfig <$> getState

getBaseDir           = confDir <$> getConfig
getVerbosity         = confVerbosity <$> getConfig
getTargetPrefixes    = confTargetPrefixes <$> getConfig
getMethodCategories  = confMethodCategories <$> getConfig
getArgTypeCategories = confArgTypeCategories <$> getConfig
getArgLimit          = confArgLimit <$> getConfig
getInitialCaps       = confInitialCaps <$> getConfig
getVerifyMethods     = confVerifyMethods <$> getConfig


