{-# LANGUAGE LambdaCase #-}
module Fluidity.EVM.Analyse.Pathfinder where

import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.List (intercalate, uncons)

import Control.Monad.Result
import Control.Monad.Resultant
--import Data.Space.StateTree

import Fluidity.EVM.Text (formatIntType)
import Fluidity.EVM.Text.Dump (dumpCompact)
import Fluidity.EVM.Data.Value (Value, cloneWith, uninitialised)
import Fluidity.EVM.Core.System (Sys)
import Fluidity.EVM.Core.VM (VM)
import Fluidity.EVM.Core.Interrupt (Interrupt, IntType)
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.Interrupt as I


data Path
  = Branch Int Path Path
  | Event  Int Path Interrupt
  | Halt   Int
  | Fail   Sys.Error Sys.State
  | Error  String
  | Loop   Int
  | Snip
  deriving (Eq, Show)

data PathPattern
  = MatchI IntType

formatPath :: Path -> String
formatPath = finish . format ([], [])
  where
    format :: ([String], [String]) -> Path -> ([String], [String])
    format (cur,acc) p = case p of
      Snip -> ("<snip>" : cur, acc)
      Error e -> 
        let lbl = "internal error: " ++ e
        in (lbl : cur, acc)
      Fail e st -> 
        let lbl = "Fail: " ++ show e ++ " \n" ++ dumpCompact st
        in (lbl : cur, acc)
      Halt x -> 
        let lbl = Format.codePtr x ++ ":Halt"
        in (lbl : cur, acc)
      Loop x -> 
        let lbl = "[" ++ Format.codePtr x ++ "]"
        in (lbl : cur, acc)
      Event x p' i  -> 
        let name = formatIntType $ I.iType i
            lbl  = Format.codePtr x ++ ":" ++ name
        in format (lbl : cur, acc) p'
      Branch x a b -> 
        let 
          lbl   = Format.codePtr x ++ " Branch(" ++ mkLabel a ++ " | " ++ mkLabel b ++ ")"
          line  = formatLine $ lbl : cur
          lines = mkLines a <> mkLines b
          mkLines c = case (c, loc c) of
            (Halt _, _)  -> []
            (Loop _, _)  -> []
            (_, Nothing) -> []
            (_, Just x)  ->
              let (l, ls) = format ([Format.codePtr x ++ " :: "], []) c
              in case l of [] -> ls
                           _  -> (formatLine l) : ls
          mkLabel p = case loc p of
            Just x  -> case p of
              Halt _  -> Format.codePtr x ++ ":Halt"
              _       -> "[" ++ Format.codePtr x ++ "]"
            Nothing -> case p of
              Error e  -> "internal error (" ++ e ++ ")"
              Fail e s -> "Fail (" ++ show e ++ " \n" ++ dumpCompact s ++ ")"
              Snip     -> "<snip>"
              Loop x   -> "[" ++ Format.codePtr x ++ "]"
        in
          ([], line : (lines ++ acc))

    finish :: ([String], [String]) -> String
    finish (cur, acc) = case cur of
      [] -> unlines acc
      _  -> unlines (formatLine cur : acc)

    formatLine xs = case reverse xs of
      []     -> "INVALID"
      t : ts -> t ++ intercalate " -> " ts


pruneLoops :: Path -> Path
pruneLoops path = prune path
  where
    nodes    = nodeList path
    clip x p = if x `elem` nodes then p else Snip
    prune p  = case p of
      Loop x       -> clip x p
      Event x a i  -> case prune a of Snip -> Snip
                                      a'   -> Event x a' i
      Branch x a b -> case (prune a, prune b) of
                        (Snip, Snip)            -> Snip
                        (Snip, b'  )            -> b'
                        (a'  , Snip)            -> a'
                        (a'  , b'  )            -> same (Branch x a' b') a' b'
      _ -> p

same :: Path -> Path -> Path -> Path
same p a b = case (a, b, loc a, loc b) of
  (_     , _     , _     , _     ) | a == b -> a
  (Loop r, _     , _     , Just q) | r == q -> b
  (_     , Loop r, Just q, _     ) | r == q -> a
  _                                         -> p

nodeList :: Path -> [Int]
nodeList = foldP (\acc -> f acc . loc) []
  where f a (Just x) = x:a
        f a _        = a

loc :: Path -> Maybe Int
loc p = case p of
  Error _      -> Nothing
  Fail _ _     -> Nothing
  Snip         -> Nothing
  Loop _       -> Nothing
  Halt x       -> Just x
  Event x _ _  -> Just x
  Branch x _ _ -> Just x
 

foldP :: (a -> Path -> a) -> a -> Path -> a
foldP f x p = case p of
  Branch _ a b -> f (foldP f (foldP f x a) b) p
  Event _ a _  -> f (foldP f x a) p
  _            -> f x p

filterErrors :: Path -> Path
filterErrors p = case p of
  Branch x a b -> case (filterErrors a, filterErrors b) of 
                    (Snip, Snip) -> Snip
                    (Snip, b'  ) -> b'
                    (a'  , Snip) -> a'
                    (a'  , b'  ) -> same (Branch x a' b') a' b'
  Event x a i  -> case filterErrors a of 
                    Snip -> Snip
                    a'   -> Event x a' i
  Halt _       -> p
  Fail _ _     -> Snip
  Error _      -> Snip
  Loop _       -> p

deduplicate :: Path -> Path
deduplicate = snd . dedupe []
  where
    dedupe acc p = 
      let
        prune x p = if x `elem` acc 
                    then (acc, Loop x)
                    else p
      in case p of
        Branch x a b -> let acc'         = x : acc
                            (acc'' , a') = dedupe acc' a
                            (acc''', b') = dedupe acc'' b
                        in x `prune` (acc''', same (Branch x a' b') a' b')
        Event x p i  -> let acc'        = x : acc
                            (acc'', p') = dedupe acc' p
                        in x `prune` case i of 
                          I.Stop     -> (acc' , Halt x)
                          I.Return _ -> (acc' , Halt x)
                          _            -> (acc'', Event x p' i)
        Halt x       -> x `prune` (x:acc, Halt x)
        _            -> (acc, p)

-- State tree generation
-- ---------------------------------------------------------------------

tracePaths :: Sys.State -> Path
tracePaths state =
  let
    state' = state { Sys.stConfig = config }
    config = Sys.Config
      { Sys.cInterrupts  = I.defaults
      , Sys.cStrategy    = Sys.Preempt
      , Sys.cBreakpoints = [] 
      }
  in
    pruneLoops . pruneLoops . pruneLoops . filterErrors . deduplicate $ explore state'
    --explore state'

-- | Build a potentially infinite tree of pathways that execution can follow
explore :: Sys.State -> Path
explore st = 
  let
    (st', r) = Sys.run Sys.resume st
    vm       = head $ Sys.stStack st'
    pc       = VM.stPC vm
    left     = exploreWith (\(x:_:xs) -> x:zero:xs) st'
    right    = exploreWith (\(x:_:xs) -> x:one:xs)  st'
    zero     = uninitialised
    one      = cloneWith (const 1) zero

    exploreWith f s = case fmap explore (updateStack f s) of
      Right x -> x
      Left e  -> Error e

    updateStack f s = update <$> updateVMs f (Sys.stStack s)
      where update x = s { Sys.stStack = x }

    updateVMs f vs = case vs of 
      v:vs' -> flip fmap (vmStack v) $ \x -> (v {VM.stStack = f x}) : vs'
      _     -> Left "empty call stack"

    vmStack v = case VM.stStack v of 
      x1:x2:xs -> Right $ x1:x2:xs
      _        -> Left "imminent stack underflow"

  in case r of
    Sys.Done _ -> Error "exploreed too deep"
    Sys.Fail e -> Fail e st'
    Sys.Susp i -> case i of
      I.Stop      -> Halt pc
      I.Return _  -> Halt pc
      I.JumpI _ _ -> Branch pc left right
      _             -> Event pc (explore st') i

--data Halt = InternalError | SystemError | Success
--type ExecTree = STree Halt [Interrupt]
--
--generate :: Sys.State -> ExecTree
--generate st =
--  let
--    mkLeft :: Sys.State -> Maybe (Int, ExecTree)
--    mkLeft st = do
--      ptr <- (+1) <$> getPC st
--      st' <- setStack2 0 st
--      return (ptr, generate st')
--
--    mkRight :: Sys.State -> Value -> Maybe (Int, ExecTree)
--    mkRight st val = do
--      let ptr = toInt val
--      st' <- setStack2 0 st
--      return (ptr, generate st')
--      
--  in case Sys.run Sys.resume st of
--    (st', Sys.Done _) -> Halt InternalError
--    (st', Sys.Fail _) -> Halt SystemError
--    (st', Sys.Susp i) -> case i of
--      I.Stop      -> Halt Success
--      I.Return _  -> Halt Success
--      I.JumpI _ p -> 
--
--getPC :: Sys.State -> Maybe Int
--getPC sys = do
--  (vm, _) <- uncons $ Sys.stStack sys
--  return $ VM.stPC vm
--
--setStack2 :: Int -> Sys.State -> Maybe Sys.State
--setStack2 x sys = do 
--  (vm, vms) <- uncons $ Sys.stStack sys
--  vmStack   <- case VM.stStack vm of
--    a:b:xs -> Just $ a:(cloneWith (const x) uninitialized):xs
--    _      -> Nothing
--  let vms' = (vm { VM.stStack = vmStack }) : vms
--  return $ sys { Sys.stStack = vms' }


