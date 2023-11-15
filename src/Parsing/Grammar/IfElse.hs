module Parsing.Grammar.IfElse (ifGrammar, ifLex) where

import Parsing.Grammar.Grammar

ifGrammar :: Grammar
ifGrammar = Grammar prods s
  where
    s = NT "S"
    s' = NT "S'"
    e  = NT "E"
    it = T "i"
    tt = T "t"
    at = T "a"
    et = T "e"
    bt = T "b"
    prods = [ s  +-> [ Symb it , Var e, Symb tt, Var s, Var s']
            , s +-> [ Symb at ]
            , s' +-> [Symb et, Var s]
            , s' +-> [Symb Lambda]
            , e  +-> [Symb bt]
            ]

-- simple function for lexing input

ifLex :: String -> Maybe (String, String)
ifLex s
  = lex' (elimSpaces s)
  where
    elimSpaces = concat . words
    wrap x = [x]
    isCharToken c = c `elem` "itaeb$"
    lex' [] = Just ([], [])
    lex' (c : cs)
      | isCharToken c = Just (wrap c, cs)
      | otherwise = Nothing
