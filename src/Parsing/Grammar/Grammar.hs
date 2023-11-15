module Parsing.Grammar.Grammar ( Terminal (..)
                               , Nonterminal (..)
                               , Symbol (..)
                               , Production (..)
                               , (+->)
                               , Grammar (..)
                               , terminals
                               , nonterminals
                               , symbols
                               , fixpoint
                               ) where

import Data.List (nub)

data Terminal
  = T String
  | Dollar
  | Lambda
  deriving (Eq, Ord)

instance Show Terminal where
  show (T s) = s
  show Dollar = "$"
  show Lambda = "lambda"

data Nonterminal
  = NT String
  deriving (Eq, Ord)

instance Show Nonterminal where
  show (NT s) = s

data Symbol
  = Var Nonterminal
  | Symb Terminal
  | Dot
  deriving (Eq, Ord)

instance Show Symbol where
  show (Var n) = show n
  show (Symb t) = show t
  show Dot = "."

data Production
  = Prod {
      leftHand :: Nonterminal
    , rightHand :: [Symbol]
    } deriving (Eq, Ord)

instance Show Production where
  show (Prod l rs) = show l ++ " -> " ++ concatMap show rs

(+->) :: Nonterminal -> [Symbol] -> Production
(+->) = Prod

data Grammar
  = Grammar {
      productions :: [Production]
    , start       :: Nonterminal
    }

instance Show Grammar where
  show g = unlines $ map show (productions g)

-- getting all terminals and nonterminals

terminals :: Grammar -> [Terminal]
terminals = symbs . concatMap rightHand . productions
  where
    symbs xs = [x | (Symb x) <- xs]

nonterminals :: Grammar -> [Nonterminal]
nonterminals = map leftHand . productions

symbols :: Grammar -> [Symbol]
symbols = nub . concatMap rightHand . productions

-- definition of a fixpoint operator

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = let x' = f x
               in if x == x' then x
                  else fixpoint f x'
