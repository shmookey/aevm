module Fluidity.EVM.Text.Util where

import Data.Char (toUpper, toLower)


-- Padding

padLeft :: Int -> String -> String
padLeft = padLeftWith ' '

padRight :: Int -> String -> String
padRight = padRightWith ' '

padLeftWith :: Char -> Int -> String -> String
padLeftWith c n xs = p ++ xs
  where
    r = n - length xs
    p = if r <= 0 then "" else take r (repeat c)

padRightWith :: Char -> Int -> String -> String
padRightWith c n xs = xs ++ p
  where
    r = n - length xs
    p = if r <= 0 then "" else take r (repeat c)

indent :: Int -> String -> String
indent n = unlines . map (pad ++) . lines
  where pad = take n $ repeat ' '


-- Capitalisation

uppercase :: String -> String
uppercase = map toUpper

lowercase :: String -> String
lowercase = map toLower

