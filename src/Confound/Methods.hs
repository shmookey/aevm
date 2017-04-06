module Confound.Methods where

import Prelude hiding (fail)
import Data.Char (toUpper, isSpace)
import Data.Maybe (isJust)
import Data.Map (Map)
import Data.List.Split (splitOn)
import Data.List (break, nub, findIndex, isInfixOf, intercalate, dropWhile, dropWhileEnd)
import Data.ByteString (ByteString)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map as Map

import Crypto.Hash as Crypto

import Control.Monad.Result
import Control.Monad.Resultant
import Confound.Types
import Confound.Util
import Fluidity.Common.Binary (unroll, padBytes)


methodNames :: Map String [String]
methodNames = Map.fromList
  [ ( "fin"
    , [ "withdraw", "deposit", "transfer", "pay", "balance", "donate", "send", "credit", "debit", "take" ]
    )
  , ( "trade"
    , [ "buy", "sell", "purchase", "refund", "bid", "offer", "trade", "give" ]
    )
  , ( "org"
    , [ "vote", "sign", "announce", "register", "request", "accept", "reject", "cancel", "approve", "deny" ]
    )
  , ( "crud"
    , [ "add", "remove", "insert", "delete", "new", "destroy", "submit", "get" ]
    )
  , ( "bet"
    , [ "bet", "wager", "gamble" ]
    )
  , ( "sys"
    , [ "open", "close", "commit" ]
    )
  ]

types :: Map String [String]
types = Map.fromList
  [ ( "minimal"
    , [ "bytes", "address", "string", "uint256" ]
    )
  , ( "basic"
    , [ "bytes", "address", "string", "uint256", "int256", "bytes32", "uint64" ]
    )
  , ( "extended"
    , [ "bytes", "address", "string"
      , "uint256", "uint64", "uint32"
      , "int256", "int64", "int32"
      , "bytes32", "bytes16", "bytes1"
      ]
    )
  , ( "broad"
    , [ "bytes", "address", "string"
      , "uint256", "uint128", "uint64", "uint32", "uint16", "uint8"
      , "int256", "int128", "int64", "int32", "int16", "int8"
      , "bytes32", "bytes16", "bytes8", "bytes4", "bytes2", "bytes1"
      ]
    )
  , ( "all"
    , [ "bytes", "address", "string"
      , "uint256", "uint128", "uint64", "uint32", "uint16", "uint8"
      , "int256", "int128", "int64", "int32", "int16", "int8"
      , "bytes32", "bytes31", "bytes30", "bytes29", "bytes28", "bytes26", "bytes25", "bytes24"
      , "bytes23", "bytes22", "bytes21", "bytes20", "bytes19", "bytes18", "bytes17", "bytes16"
      , "bytes15", "bytes14", "bytes13", "bytes12", "bytes11", "bytes10", "bytes9",  "bytes8"
      , "bytes7",  "bytes7",  "bytes6",  "bytes5",  "bytes4",  "bytes3",  "bytes2",  "bytes1"
      ]
    )
  ]

encodeCall :: String -> Result String String
encodeCall call =
  let
    dropLast :: [a] -> [a]
    dropLast = reverse . drop 1 . reverse

    strip :: String -> String
    strip = dropWhileEnd isSpace . dropWhile isSpace

    readArg :: String -> Result String (String, String)
    readArg x = case splitOn ":" x of
      [rawVal, rawType] -> Ok (strip rawVal, strip rawType)
      _ -> Err $ "invalid argument: " ++ x

    parseArgs :: String -> Result String [(String, String)]
    parseArgs argStr = 
      let xs = splitOn "," . drop 1 . dropLast $ strip argStr
      in case xs of [""] -> return []
                    []   -> return []
                    _    -> mapM readArg xs


    encodeArg :: String -> String -> Result String String
    encodeArg val typ = case typ of
     "uint256" -> Ok . B8.unpack . B16.encode . padBytes 32 $ unroll (read val :: Integer)
     "address" -> Ok . B8.unpack . B16.encode . padBytes 32 . fst . B16.decode $ B8.pack val
     _         -> Err $ "encoding values of type " ++ typ ++ " is not supported"

    (namePart, argPart) = break (== '(') call
    method = strip namePart

  in do
    args    <- parseArgs argPart
    let hash = methodHash . makeSig method $ map snd args
    argData <- concat <$> mapM (uncurry encodeArg) args
    Ok $ hash ++ argData


makeSig :: String -> [String] -> String
makeSig method args =
  method ++ "(" ++ (intercalate "," args) ++ ")"

methodHash :: String -> String
methodHash x = 
  take 8 $ show (Crypto.hash (B8.pack x) :: Crypto.Digest Crypto.Keccak_256)
 
methodHash' :: String -> ByteString
methodHash' x = 
  B8.take 4 $ BA.convert (Crypto.hash (B8.pack x) :: Crypto.Digest Crypto.Keccak_256)
 
createMethodList :: App [(String, MethodID)]
createMethodList =
  let
    methodNamesByCategory :: [String] -> App [String]
    methodNamesByCategory ["all"] = return . concat $ Map.elems methodNames
    methodNamesByCategory xs      = concat <$> mapM (flip mapGet methodNames) xs

    argTypesByCategory :: [String] -> App [String]
    argTypesByCategory cats = concat <$> mapM (flip mapGet types) cats

    initialCaps :: String -> String
    initialCaps (x:xs) = (toUpper x):xs
     
    combinations :: Int -> [String] -> [String] -> [String]
    combinations argLimit validMethods validArgs = do
      method <- validMethods
      n      <- [0 .. argLimit]
      args   <- if n == 0 then [[]] else (mapM (const validArgs) [1 .. n])
      return $ makeSig method args

  in do
    validMethods <- nub <$> (getMethodCategories >>= methodNamesByCategory)
    validArgs    <- nub <$> (getArgTypeCategories >>= argTypesByCategory)
    argLimit     <- getArgLimit
    initCaps     <- getInitialCaps
    
    let validMethods' = if initCaps 
                        then validMethods ++ (map initialCaps validMethods)
                        else validMethods

    let methodSigs = combinations argLimit validMethods' validArgs

    logInfo $ "Generated " ++ (show $ length methodSigs) ++ " method signatures"
    return . zip methodSigs $ map methodHash methodSigs
    
containsMethod :: Bytecode -> (String, MethodID) -> Bool
containsMethod bc (_, x) = isInfixOf x bc

identifyMethods :: [(String, MethodID)] -> Contract -> App [(String, MethodID)]
identifyMethods methods contract =
  let
    matches  = filter (containsMethod $ ctBytecode contract) methods
    cid      = ctID contract
    disasm   = ctDisassembly contract

    instNumber :: String -> Int
    instNumber x = read x
    
    methodIndex :: ASM -> String -> Maybe Int
    methodIndex code x = do
      i <- findIndex (isInfixOf x) code
      let area = take 6 $ drop i code
      j <- findIndex (isInfixOf "JUMPI") area
      let line = area !! (j - 1)
      let loc = instNumber . last $ words line
      return loc

  in do
    verify <- getVerifyMethods
    let matches' = if verify
                   then filter (isJust . methodIndex disasm . snd) matches
                   else matches
    
    let matchStr = unwords $ map fst matches'
                
    case matches' of
      [] -> return matches'
      _  -> logInfo (cid ++ " " ++ matchStr) >> return matches'




