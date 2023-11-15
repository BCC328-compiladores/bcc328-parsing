module Parsing.LR.Zero.Itens where

import Data.List
import Parsing.Grammar.Grammar

newtype Itens = Itens { prods :: [Production] }

instance Show Itens where
  show = unlines . map show . prods

instance Eq Itens where
  (Itens its1) == (Itens its2) = sublist its1 its2 && sublist its2 its1

sublist :: Eq a => [a] -> [a] -> Bool
sublist xs ys = all (\ x -> x `elem` ys) xs

-- function for building the set of LR(0) itens

itens :: Grammar -> [Itens]
itens g = fixpoint go [closure g' cinit]
  where
    g' = extendedGrammar g
    cinit = Itens [putDot $ head $ productions g']
    go c  = nub $ c `union` [goto g' it s | it <- c, s <- symbols g', s /= Dot]


-- definition of the closure.

closure :: Grammar -> Itens -> Itens
closure g = fixpoint go
  where
    go its =
      let
        (nexts, ended) = partition (canExpand isVar) (prods its)
        nts = nub $ map varToExpand nexts
        new = filter (\ p -> (leftHand p) `elem` nts) (productions g)
        new' = map putDot new
        curr = ended `union` nexts `union `new'
      in Itens curr

varToExpand :: Production -> Nonterminal
varToExpand (Prod _ rhs) = go rhs
  where
    go ss = case span (/= Dot) ss of
              (_, Dot : (Var s) : _) -> s
              _                      -> error "impossible"

putDot :: Production -> Production
putDot (Prod v rhs) = Prod v (Dot : rhs)

canExpand :: (Symbol -> Bool) -> Production -> Bool
canExpand p prod
  = case span (/= Dot) (rightHand prod) of
      (_, Dot : s : _) -> p s
      _ -> False

isVar :: Symbol -> Bool
isVar (Var _) = True
isVar _       = False

-- definition of the goto function

goto :: Grammar -> Itens -> Symbol -> Itens
goto g its sym
  = let
      (nexts, _) = partition (canExpand (== sym)) (prods its)
      news = map moveDot nexts
    in closure g (Itens news)

moveDot :: Production -> Production
moveDot p@(Prod v rhs)
  = case span (/= Dot) rhs of
      (prefs, Dot : x : ss)  ->
        Prod v $ prefs ++ x : Dot : ss
      _ -> p

-- building the extended grammar

extendedGrammar :: Grammar -> Grammar
extendedGrammar (Grammar ps s)
  = Grammar prods' s'
  where
    s' = NT (show s ++ "'")
    prods' = (s' +-> [Var s]) : ps

-- print the itens for a grammar

debugItens :: Grammar -> IO ()
debugItens g
  = do
      let its = zip [1..] (filter (not . null . prods) $ itens g)
      putStrLn "Itens"
      mapM_ printItem its

printItem :: (Int, Itens) -> IO ()
printItem (i, ts)
  = do
      putStrLn $ "Item set " ++ show i ++ ":"
      mapM_ print (prods ts)


-- simple example grammar

gram :: Grammar
gram = Grammar prods1 ns
  where
    ns = NT "S"
    nl = NT "L"
    lparen = T "("
    rparen = T ")"
    x = T "x"
    comma = T ","
    prods1 = [ ns +-> [ Symb lparen
                     , Var nl
                     , Symb rparen
                     ]
            , ns +-> [Symb x]
            , nl +-> [ Var nl
                     , Symb comma
                     , Var ns
                     ]
            , nl +-> [ Var ns ]
            ]
