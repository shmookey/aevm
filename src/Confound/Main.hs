module Main where

import Data.Char (toUpper)
import Data.List (intercalate, isInfixOf)
import Data.ByteString.Char8 (ByteString, pack) 
import Crypto.Hash
import System.Environment
import Control.Monad

import Analyse

types = 
     [ "bytes", "address", "string", "uint256", "int256" ]
--  ++ intTypes
--  ++ bytesTypes

intTypes = do
  prefix <- ["u",""]
  suffix <- map show $ map (2^) [3, 4, 5, 6]
  return $ prefix ++ "int" ++ suffix

bytesTypes = do
  n <- map show [1, 8, 16, 32]
  return $ "bytes" ++ n

baseNames = ["withdraw", "deposit", "vote", "sign", "offer", "buy", "sell", "refund", "accept", "reject", "close", "open", "add", "delete", "remove", "request", "insert", "announce", "bet", "cancel"]
leadingCaps (x:xs) = (toUpper x) : xs

names = baseNames ++ (map leadingCaps baseNames)
  
fuzz n = do
  name <- names
  args <- intercalate "," <$> mapM (const types) [1..n]
  return $ name ++ "(" ++ args ++ ")"

main :: IO ()
main =
  let
    sigs = map withHash $ concatMap fuzz [0..3]
    
    analyseMatch :: ASM -> (String, String) -> IO ()
    analyseMatch code (h, sig) = 
      let
        pos = methodIndex code h
      in case pos of
         Just x -> putStrLn $ h ++ " " ++ (show x) ++ " " ++ sig
         Nothing -> putStrLn $ h ++ " " ++ sig ++ ": not found"
      

    checkFile :: FilePath -> IO ()
    checkFile p = do
      bytecode <- readFile p
      let asm = lines bytecode
      let matches = filter (flip isInfixOf bytecode . fst) sigs
      when (length matches > 0) . putStrLn $ p ++ " " ++ (show $ length matches) ++ " " ++ (intercalate " " . map snd $ matches)
      if (length matches > 0)
      then do
        mapM_ (analyseMatch asm) matches
        putStrLn "\n"
      else
        return ()

      

  in do
    filenames <- getArgs
    mapM_ checkFile filenames



