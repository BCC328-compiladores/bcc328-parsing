module Parsing.LR.Action where

import Parsing.Grammar.Grammar

data Action
  = Accept
  | Error
  | Shift Symbol
  | Reduce Production
  deriving Eq

instance Show Action where
  show Accept = "accept"
  show Error = "error"
  show (Shift s) = "shift " ++ show s
  show (Reduce p) = "reduce " ++ show p
