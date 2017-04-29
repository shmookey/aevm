module Fluidity.Common.ANSI where

import System.Console.ANSI
import Data.List.Split (splitOn)
import Data.List (dropWhile)
import qualified Data.Text as T

import Text.Structured (Structured(fmt), block, typeset, (~-), (~~))
import qualified Text.Structured as TS


startHighlight c = setSGRCode [SetColor Foreground Vivid c]
endHighlight     = setSGRCode [Reset]

highlight :: Structured a => Color -> a -> TS.Fragment
highlight c x = startHighlight c ~~ x ~~ endHighlight

colour :: Structured a => Color -> a -> TS.Fragment
colour c x = setSGRCode [SetColor Foreground Dull c] ~~ x ~~ endHighlight

embolden :: Structured a => a -> TS.Fragment
embolden x = setSGRCode [SetConsoleIntensity BoldIntensity] ~~ x ~~ setSGRCode [SetConsoleIntensity NormalIntensity]

-- black red green yellow blue magenta cyan white


colouringWheel :: [String -> String]
colouringWheel = cycle $ map ((.) TS.toString)
  [ f c | c <- [Green, Yellow, Red, Blue, Magenta, Cyan]
        , f <- [colour, highlight]
  ]


emph :: String -> String
emph = TS.toString . embolden . highlight White


stripColours :: String -> String
stripColours str = case splitOn "\ESC" str of
  []     -> ""
  x : [] -> x
  x : xs -> x ++ (concat $ map (drop 1 . dropWhile (/= 'm')) xs)

