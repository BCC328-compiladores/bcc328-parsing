module Parsing.LR.Zero.BuildTable where

import Parsing.Grammar.Grammar
import Parsing.LR.Action
import Parsing.LR.Zero.Itens

-- Definition of table

type Table = [((Itens, Symbol), (Action, Itens))]

buildTable :: Grammar -> ([Itens], Table)
buildTable g
  = fixpoint (stepTable g') ([t], [])
  where
      g' = extendedGrammar g
      t = Itens [putDot $ head $ productions g']

stepTable :: Grammar -> ([Itens], Table) -> ([Itens], Table)
stepTable g (t, e) = undefined

-- checking if a production is finished

finished :: Production -> Bool
finished p = last (rightHand p) == Dot
