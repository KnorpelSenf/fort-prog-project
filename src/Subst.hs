module Subst
  ( Subst(..), empty, single, apply, mapApply, compose )
where

-- Module for Task 2

import Type ( Term(..), VarIndex )

-- Datatype representing a Substitution
data Subst = Subst [(VarIndex, Term)]
  deriving Show

empty :: Subst
empty = Subst []

single :: VarIndex -> Term -> Subst
single vi t = Subst [(vi,t)]

apply :: Subst -> Term -> Term
apply (Subst [])           t           = t
apply (Subst ((sv,st):ss)) t@(Var v)   = apply (Subst ss) (if sv == v then st else t)
  -- if sv == v 
  -- then apply (Subst ss) st 
  -- else apply (Subst ss) t
apply s                    (Comb p as) = Comb p (mapApply s as)

-- apply subst to multiple terms
mapApply :: Subst -> [Term] -> [Term]
mapApply s ts = map (apply s) ts

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s0) = Subst
                                . (s0 ++)
                                . filter (\(v, _) -> not
                                                     . (v `elem`)
                                                     . fst
                                                     . unzip
                                                     $ s0)
                                $ s1

{-
                                let mayApp (v, _) = not
                                                    . elem v
                                                    . fst
                                                    . unzip
                                                    $ s0
                                in Subst (s0 ++ (filter mayApp s1))
-}

--testCompose :: Subst -> Subst -> Term -> Bool
--testCompose s2 s1 t = apply (compose s2 s1) t == apply s2 (apply s1 t)
