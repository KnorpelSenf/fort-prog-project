module Strategy
  ( Strategy, dfs, bfs, solve )
where

--Module for Task 5

import Type
import Subst ( Subst(..), empty, compose, apply )
import SLD   ( SLDTree(..), sld, varsIn         )

type Strategy = SLDTree -> [Subst]

--  Depth first search
dfs :: Strategy
dfs = dfs' empty
  where
    dfs' :: Subst -> Strategy
    dfs' s (Node (Goal []) _    ) = [s]
    dfs' _ (Node _         []   ) = []
    dfs' s (Node _         edges) = concat
                                    . map (uncurry dfs')
                                    . map (\(st,t) -> ((compose st s), t)) 
                                    $ edges

-- Breadth first search
bfs :: Strategy
bfs = bfs' empty []
  where
    bfs' :: Subst -> [SLDTree] -> Strategy
    bfs' s q (Node _ c) = let isSolution = \sldTree -> (case sldTree of
                                                         Node (Goal []) _ -> True
                                                         _                -> False)
                              children   = map (\(st,t) -> (compose st s,t)) c
                              f          = isSolution . snd
                              solutions  = fst . unzip . (filter f)         $ children
                              queue      = snd . unzip . (filter (not . f)) $ children
                          in solutions ++ concatMap (\(sub,t) -> bfs' sub (q ++ queue) t) children

solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g@(Goal ts) = map filterSubst
                        . s
                        . sld p
                        $ g
  where
    filterSubst :: Subst -> Subst
    filterSubst subst = Subst
                        . map (\vi -> (vi, apply subst $ Var vi))
                        . concatMap varsIn
                        $ ts
