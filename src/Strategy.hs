module Strategy where
--Module for Task 5

import SLD
import Type
import Subst
import Data.List

type Strategy = SLDTree -> [Subst]

--  Depth first search
dfs :: Strategy
dfs = dfs' empty
  where
    dfs' :: Subst -> Strategy
    dfs' s (Node (Goal []) _       ) = [s]
    dfs' _ (Node _         []      ) = []
    dfs' s (Node _         children) = concatMap (\(st,t) -> dfs' (compose s st) t) children

-- Breadth first search
bfs :: Strategy
bfs = bfs' empty []
  where
    bfs' :: Subst -> [SLDTree] -> Strategy
    bfs' s q (Node _ c) = let isSolution = \sld -> (case sld of
                                                         Node (Goal []) _ -> True
                                                         _                -> False)
                              children   = map (\(st,t) -> (compose s st,t)) c
                              f          = isSolution . snd
                              solutions  = fst . unzip . (filter f)         $ children
                              queue      = snd . unzip . (filter (not . f)) $ children
                          in solutions ++ concatMap (\(s,t) -> bfs' s (q ++ queue) t) children
      -- Kinder holen
      -- filtern nach Lösung (also nach Goal [])
      -- diese an den Akkumulator hängen
      -- mit dem Komplement die Rekursion fortsetzen

solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g@(Goal ts) = map filterSubst (s (sld p g))
  where
    filterSubst :: Subst -> Subst
    filterSubst subst = Subst
                          $ map (\vi -> (vi, apply subst $ Var vi))
                          $ concatMap varsIn ts
