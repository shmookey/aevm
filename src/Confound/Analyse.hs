module Analyse where



import Data.ByteString.Char8 (ByteString, pack) 
import Crypto.Hash
import Data.List -- (intercalate, isInfixOf)

type ASM = [String]


methodId :: ByteString -> String
methodId bs = take 8 $ show (hash bs :: Digest Keccak_256)

withHash :: String -> (String, String)
withHash x = (methodId $ pack x, x)


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

methodImpl :: 

