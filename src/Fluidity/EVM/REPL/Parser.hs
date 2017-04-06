module Fluidity.EVM.REPL.Parser where

import Control.Monad (void)
import Control.Applicative ((<|>), (<*))
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Identity (Identity)
import Text.Parsec ((<?>))
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec as P

import Control.Monad.Result

import Fluidity.Common.Binary (fromHex, fromHexOnly)
import Fluidity.EVM.REPL.Command


type Parse a = P.Parsec TokenStream () a
type TokenStream = [(Lexeme, P.SourcePos)]
data Error 
  = ParseError Int String TokenStream 
  | LexError Int String
  deriving (Show)


parse :: String -> Result Error Command
parse str = lexCommand str >>= parseCommand

parseCommand :: TokenStream -> Result Error Command
parseCommand toks =
  let
    output = P.runParser command () "REPL" toks
    getCol = P.sourceColumn . P.errorPos
  in case output of
    Left err -> Err $ ParseError (getCol err) (show err) toks
    Right x  -> Ok x

command :: Parse Command
command = P.choice
  [ Chain    <$> cmdChain
  , EVM      <$> cmdEVM
  , Meta     <$> cmdMeta
  , State    <$> cmdState
  , Parallel <$> cmdPar
  ] <* P.eof


-- EVM Commands
-- ---------------------------------------------------------------------

cmdEVM :: Parse EVM
cmdEVM = 
  let
    cmdInspect :: Parse Inspect
    cmdInspect = do
      keyword "inspect"
      space
      P.choice
        [ const InspectStack   <$> keyword "stack"  
        , const InspectMemory  <$> keyword "memory" 
        , const InspectStorage <$> keyword "storage" 
        , const InspectCall    <$> keyword "call"
        , do keyword "code"
             ref <- P.optionMaybe (space >> codeRef)
             return $ InspectCode ref
        ]

  in do
    keyword "evm"
    space
    P.choice
      [ const Go   <$> keyword "go"
      , const Step <$> keyword "step"
      , do keyword "break"
           space
           x <- litInt
           return $ BreakAt x
      , cmdCall
      , Inspect <$> cmdInspect
      ]


-- Message calls
-- ---------------------------------------------------------------------

cmdCall :: Parse EVM
cmdCall = do
  keyword "call"
  space
  addr  <- address
  space
  val   <- litInt
  gas   <- P.optionMaybe (space >> litInt)
  cdata <- P.optionMaybe (space >> callData)
  return $ Call addr val gas cdata

callData :: Parse CallData
callData = P.choice
  [ do bs <- P.try hex
       return $ RawCall bs
  , do name <- word
       optionalSpace
       openParen
       optionalSpace
       args <- P.sepBy methodArg commaSpace
       optionalSpace
       closeParen
       return $ MethodCall name args
  ]

methodArg :: Parse MethodArg
methodArg = P.choice $ map P.try
  [ do t <- word
       space
       x <- litInt
       return $ NumArg x t
  , do keyword "bool"
       space
       val <- (const True <$> keyword "true")
          <|> (const False <$> keyword "false")
       return $ BoolArg val
  , do keyword "address"
       space
       bs <- hex
       return $ AddrArg bs 
  ]


-- Parallel commands
-- ---------------------------------------------------------------------

cmdPar :: Parse Parallel
cmdPar = do
  keyword "par"
  space
  P.choice
    [ do keyword "call"
         space
         ref   <- setRef
         space
         val   <- litInt
         gas   <- P.optionMaybe $ P.try (space >> litInt)
         cdata <- P.optionMaybe $ P.try (space >> callData)
         post  <- P.optionMaybe $ space >> postProcess
         return $ ParCall ref val gas cdata post
    , do keyword "set"
         space
         ParSets <$> P.choice
            [ do keyword "list"
                 return ParSetList
            , do keyword "show"
                 space
                 x <- word
                 return $ ParSetShow x
            , do keyword "showstorage"
                 space
                 x <- word
                 return $ ParSetShowStorage x
            , do keyword "drop"
                 space
                 x <- word
                 return $ ParSetDrop x
            ]
    ]


-- State commands
-- ---------------------------------------------------------------------

cmdState :: Parse State
cmdState = do
  keyword "state"
  space
  P.choice
    [ const StateSave <$> keyword "save"
    , const StateList <$> keyword "list"
    , do keyword "load"
         space
         x <- litInt
         return $ StateLoad x
    , do keyword "drop"
         space
         x <- litInt
         return $ StateDrop x
    ]

-- Chain commands
-- ---------------------------------------------------------------------

cmdChain :: Parse Chain
cmdChain = 
  let
    chainBlock :: Parse Block
    chainBlock = keyword "block" >> space >> P.choice
      [ const BlockCommit <$> keyword "commit"
      , const BlockList <$> keyword "list"
      , do keyword "show"
           space
           i <- P.optionMaybe litInt
           return $ BlockShow i
      ]

    chainAccount :: Parse Account
    chainAccount = keyword "account" >> space >> P.choice
      [ do keyword "list"
           prefix <- P.optionMaybe (space >> address)
           return $ AccountList prefix
      , do keyword "show"
           space
           addr <- address
           return $ AccountShow addr
      , do keyword "drop"
           space
           addr <- address
           return $ AccountDrop addr
      , do keyword "balance"
           space
           P.choice
             [ do keyword "get"
                  space
                  addr <- address
                  return $ AccountBalanceGet addr
             , do keyword "set"
                  space
                  addr <- address
                  space
                  val <- litInt
                  return $ AccountBalanceSet addr val
            ]
      , do keyword "storage"
           space
           P.choice
             [ do keyword "get"
                  space
                  addr <- address
                  return $ AccountStorageGet addr
             , do keyword "getkey"
                  space
                  addr <- address
                  space
                  key <- hex
                  return $ AccountStorageGetKey addr key
             , do keyword "setkey"
                  space
                  addr <- address
                  space
                  key <- hex
                  space
                  val <- hex
                  return $ AccountStorageSetKey addr key val
            ]
      ]

  in do
    keyword "chain"
    space
    P.choice
      [ ChainBlock <$> chainBlock
      , ChainAccount <$> chainAccount
      ]


-- Meta commands
-- ---------------------------------------------------------------------

cmdMeta :: Parse Meta
cmdMeta = do
  colon
  P.choice
    [ const Quit <$> keyword "quit"
    , do keyword "help"
         topic <- P.option HelpDefault (HelpTopic <$> (space >> word))
         return $ Help topic
    ]


-- Specifiers
-- ---------------------------------------------------------------------


codeRef :: Parse CodeRef
codeRef = do
  addr <- P.optionMaybe address
  sect <- P.optionMaybe slice
  case (addr, sect) of
    (Nothing, Nothing) -> fail "coderef"
    _                  -> return (addr, sect)

address :: Parse Address
address = P.choice . map P.try $
  [ do bs <- hex
       star
       return $ Prefix bs
  , do bs <- hex
       return $ Address bs
  , do star
       return $ Prefix mempty
  ]
    
slice :: Parse Slice
slice = do
  openBracket
  optionalSpace
  start <- P.optionMaybe litInt
  optionalSpace
  colon
  optionalSpace
  end <- P.optionMaybe litInt
  optionalSpace
  closeBracket
  return (start, end)

setRef :: Parse SetRef
setRef = P.choice
  [ at >> SetAlias <$> word
  , SetRange <$> address
  ]

postProcess :: Parse PostProcess
postProcess = do
  filters <- P.optionMaybe $ do
    pipe
    optionalSpace
    P.sepEndBy filterType space
  optionalSpace
  saveto <- P.optionMaybe $ do
    greaterThan
    optionalSpace
    word
  case filters of 
    Just xs -> return (xs, saveto)
    Nothing -> return ([], saveto)
  
filterType :: Parse Filter
filterType = P.choice
  [ const FilterSStore    <$> keyword "sstore"
  , const FilterSLoad     <$> keyword "sload"
  , const FilterThrow     <$> keyword "throw"
  , const FilterOk        <$> keyword "ok"
  , const FilterErr       <$> keyword "err"
  , const FilterCall      <$> keyword "call"
  , const FilterSend      <$> keyword "send"
  , const FilterNotImpl   <$> keyword "notimpl"
  ]


-- Tokens
-- ---------------------------------------------------------------------

keyword      = exactly . Word
colon        = exactly Colon
comma        = exactly Comma
at           = exactly At
dot          = exactly Dot
space        = exactly Space
star         = exactly Star
pipe         = exactly Pipe
openParen    = exactly OpenParen
closeParen   = exactly CloseParen
openBracket  = exactly OpenBracket
closeBracket = exactly CloseBracket
openBrace    = exactly OpenBrace
closeBrace   = exactly CloseBrace
openAngle    = exactly OpenAngle
closeAngle   = exactly CloseAngle
lessThan     = exactly OpenAngle
greaterThan  = exactly CloseAngle
litInt       = token $ \t -> case t of 
                 LitInt x -> Just x
                 _        -> Nothing
litHex       = token $ \t -> case t of 
                 LitHex x -> Just x
                 _        -> Nothing
word         = token $ \t -> case t of
                 Word x   -> Just x
                 _        -> Nothing

optionalSpace = P.optional space
commaSpace = (P.try (optionalSpace >> comma >> optionalSpace)) <?> "comma"
colonSpace = (P.try (optionalSpace >> colon >> optionalSpace)) <?> "colon"

hex = (P.try (nakedHex <|> litHex)) <?> "even-length hex string"

nakedHex :: Parse ByteString
nakedHex = token $ \t -> case t of
  Word x -> fromHexOnly x
  _      -> Nothing

exactly :: Lexeme -> Parse ()
exactly x = token $ \t -> if t == x then Just () else Nothing

token :: (Lexeme -> Maybe a) -> Parse a
token f = 
  let 
    showTok (t,pos)     = show t
    posFromTok (t,pos)  = pos
    testTok (t,pos)     = f t
  in
    P.token showTok posFromTok testTok




-- Lexer
-- ---------------------------------------------------------------------


data Lexeme
  -- Literals
  = Word String
  | LitInt Integer
  | LitHex ByteString
  -- Delimiting tokens
  | At
  | Colon
  | Comma
  | Dot
  | Space
  | Star
  | Pipe
  -- Enclosing tokens
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | OpenAngle
  | CloseAngle
  deriving (Show, Eq)

type Lexer a = P.Parsec String () a

lexCommand :: String -> Result Error [(Lexeme, P.SourcePos)]
lexCommand str =
  let
    output = P.runParser (lexemes <* P.eof) () "REPL" str
    getCol = P.sourceColumn . P.errorPos
  in case output of
    Left err -> Err $ LexError (getCol err) (show err)
    Right x  -> Ok x

lexemes :: Lexer [(Lexeme, P.SourcePos)]
lexemes =
  let
    at           = tok "@" $ char '@' At 
    colon        = tok ":" $ char ':' Colon
    comma        = tok "," $ char ',' Comma
    dot          = tok "." $ char '.' Dot
    star         = tok "*" $ char '*' Star
    pipe         = tok "|" $ char '|' Pipe
    space        = tok "whitespace" $ const Space <$> (PC.space >> PC.spaces)
    openParen    = tok "(" $ char '(' OpenParen
    closeParen   = tok ")" $ char ')' CloseParen
    openBracket  = tok "[" $ char '[' OpenBracket
    closeBracket = tok "]" $ char ']' CloseBracket
    openBrace    = tok "{" $ char '{' OpenBrace
    closeBrace   = tok "}" $ char '}' CloseBrace
    openAngle    = tok "<" $ char '<' OpenAngle
    closeAngle   = tok ">" $ char '>' CloseAngle
    litInt       = tok "integer" $ do
                     ds <- P.many1 PC.digit
                     P.lookAhead eot
                     return . LitInt $ read ds
    litHex       = tok "hex literal" . P.try $ do
                     PC.string "0x"
                     xs <- P.many1 PC.hexDigit
                     P.lookAhead eot
                     return . LitHex $ fromHex xs
    word         = tok "word" $ do
                     xs <- P.many1 PC.alphaNum
                     return $ Word xs

    -- Lookahead for end-of-term  (i.e. a space or eof)
    eot :: Lexer ()
    eot = void $ P.choice
      [ P.eof           , void space
      , void openParen  , void closeParen
      , void openBracket, void closeBracket
      , void openBrace  , void closeBrace
      , void openAngle  , void closeAngle
      ]
 
    char :: Char -> Lexeme -> Lexer Lexeme
    char c t = const t <$> PC.char c

    tok :: String -> Lexer Lexeme -> Lexer (Lexeme, P.SourcePos)
    tok exp p = P.try (do { i <- P.getPosition ; x <- p ; return (x, i) } <?> exp)

  in P.many $ P.choice
    [ at, colon, comma, dot, space, star, pipe
    , openParen, closeParen, openBracket, closeBracket
    , openBrace, closeBrace, openAngle, closeAngle
    , litHex, litInt, word 
    ]


