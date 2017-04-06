module Fluidity.EVM.Bytecode where

import Fluidity.EVM.Types


isCData :: Op -> Bool
isCData op = case op of
  CData _ -> True
  _       -> False


