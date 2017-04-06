{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Structured where

import Prelude hiding (lines, unlines, unwords)
import qualified Data.Text as T
import Data.List (intersperse)
import Data.Text (Text, intercalate, lines, pack, snoc, singleton, unpack)
import Text.Printf (printf)


class Structured a where
  fmt :: a -> Fragment

  typeset :: a -> Text
  typeset = render . fmt

instance Structured Fragment where
  fmt = id

instance Structured Text where
  fmt "" = Empty
  fmt x  = Inline x

instance Structured Char where
  fmt = fmt . singleton

instance Structured a => Structured [a] where
  fmt [] = Empty
  fmt xs = Construct $ map fmt xs 

instance Structured Bool where
  fmt = fmt . show

instance Structured Int where
  fmt = fmt . show

(~~) :: (Structured a, Structured b) => a -> b -> Fragment
x ~~ y = Construct [fmt x, fmt y]

(~-) :: (Structured a, Structured b) => a -> b -> Fragment
x ~- y = case (fmt x, fmt y) of
  (Empty, Empty) -> Empty
  (x'   , Empty) -> x'
  (Empty, y'   ) -> y'
  (x'   , y'   ) -> Construct [x', fmt ' ', y']

text :: Text -> Fragment
text = fmt

block :: Structured a => [a] -> Fragment
block [] = Empty
block xs = Block . filter (not . (==) Empty) $ map fmt xs

lone :: Structured a => a -> Fragment
lone = Deisolate . fmt

iso :: Structured a => a -> Fragment
iso = Isolate . fmt

-- | A list of elements to be displayed separated by a space
phrase :: Structured a => [a] -> Fragment
phrase [] = Empty
phrase xs = fmt . intersperse (fmt ' ') $ map fmt xs

-- | A list of elements to be displayed separated by a blank line
group :: Structured a => [a] -> Fragment
group [] = Empty
group xs = Group $ map fmt xs

collect :: Structured a => [a] -> Fragment
collect [] = Empty
collect xs = Collect $ map fmt xs

-- | Create a multi-collapsing structure, i.e. if one collapses, they all do.
collapses :: (Structured a, Structured b) => [(a, b)] -> Fragment
collapses [] = Empty
collapses xs = MultiCollapse $ map (\(x,y) -> (fmt x, fmt y)) xs

-- | Create a structure that collapses into an indented sub-structured if it can't fit on one line.
collapse :: (Structured a, Structured b) => a -> b -> Fragment
collapse x y = Collapse (fmt x) (fmt y)

-- | `digits n` left-pads a number with 0s to be `n` characters wide
digits :: Int -> Int -> Fragment
digits n x =
  let pad = take (n - (length $ show x)) $ repeat '0'
  in pad ~~ x

-- | `hexdigits n` left-pads a hexadecimal number with 0s to be `n` characters wide
hexdigits :: Int -> Int -> Fragment
hexdigits n x = 
  let s :: String
      s = printf ("%0" ++ (show n) ++ "x") x
  in fmt s


-- | A fragment of loosely-structured text
data Fragment
  = Inline    Text        -- Display anywhere in a line
  | Isolate   Fragment    -- Parenthesise when adjacent to other Isolates
  | Block     [Fragment]  -- Display as lines (newline-separated)
  | Group     [Fragment]  -- Display as paragraphs (separated by blank lines)
  | Construct [Fragment]  -- Display unseparated
  | Collapse Fragment Fragment
  | MultiCollapse [(Fragment, Fragment)]
  | Collect [Fragment]
  | Deisolate Fragment
  | Empty
  deriving (Eq, Show)

textFragment :: Text -> Fragment
textFragment = Inline

render :: Fragment -> Text
render frag = 
  let
    -- | Attempt to render on a single line
    inline :: Fragment -> Maybe Text
    inline fragment = case fragment of
      Inline x      -> Just x
      Construct xs  -> tconcat <$> mapM inline xs
      Isolate x     -> parens  <$> inline x
      Block xs      -> Nothing
      Group xs      -> Nothing
      Collect xs    -> unwords <$> mapM inline xs
      Collapse x y  -> case (inline x, inline y) of
        (Just x', Just y') -> Just $ unwords [x', y']
        _                  -> Nothing
      MultiCollapse xys -> 
        unwords <$> mapM (inline . uncurry Collapse) xys
      Deisolate (Isolate x) -> inline x
      Deisolate x           -> inline x
      Empty                 -> Just ""

    -- | Render without inlining
    multiline :: Fragment -> Text
    multiline fragment = case fragment of
      Inline x      -> x
      Isolate x     -> parens $ snoc (render x) '\n'
      Construct xs  -> tconcat $ map render xs
      Block xs      -> unlines $ map render xs
      Group xs      -> unlines $ map (flip snoc '\n' . render) xs
      Collect xs    -> unlines $ map render xs
      Collapse x y  -> unlines [render x, indent 2 $ render y]
      MultiCollapse xys ->
        unlines $ map (multiline . uncurry Collapse) xys
      Empty         -> ""

    lineSep :: [Fragment] -> Text
    lineSep xs =
      let 
        item c@(Block _) = snoc (render c) '\n'
        item c           = render c
      in
        unlines $ (map item $ init xs) ++ [render $ last xs]

    format :: Fragment -> Text
    format x = case inline x of
      Just line | T.length line < 40 -> line
      _ -> multiline frag

  in case frag of
    Deisolate (Isolate x) -> render x
    Deisolate x           -> render x
    _                     -> format frag

tconcat = T.concat
unlines = T.init . T.unlines
unwords = T.unwords . filter ((> 0) . T.length)

-- | Surround text with parenthesis
parens :: Text -> Text
parens x = tconcat ["(", x, ")"]

indent :: Int -> Text -> Text
indent lvl =
  let margin = pack . take lvl $ repeat ' '
  in unlines . map (T.append margin) . lines

-- | Format a block of text with line numbers
withLineNumbers :: Text -> Text
withLineNumbers =
  -- TODO: Avoid converting to and from String
  let add :: (Int, String) -> String
      add (i, x) = printf "%3i.  %s" i x
  in unlines . map (pack . add) . zip [1..] . map unpack . lines

