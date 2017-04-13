module Fluidity.Common.ANSI where

import System.Console.ANSI
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

