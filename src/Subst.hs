module Subst where
-- Module for Task 2

import Type
import Data.List

-- Datatype representing a Substitution
--type Subst = VarIndex -> Term
data Subst = Subst [(VarIndex, Term)]
  deriving Show
--data Subst = Subst (VarIndex -> Term) [VarIndex]

empty :: Subst
--empty = Subst (\vi -> Var vi) []
empty = Subst []

single :: VarIndex -> Term -> Subst
--single vi t = Subst (\vin -> if vin == vi then t else (Var vin)) [vi]
single vi t = Subst [(vi,t)]

apply :: Subst -> Term -> Term
--apply (Subst s _) (Var vi)  = s vi
--apply s (Comb p args)       = Comb p (map (apply s) args)
apply (Subst [])           t           = t
apply (Subst ((sv,st):ss)) t@(Var v)   = if sv == v then apply (Subst ss) st else (apply (Subst ss) t)
apply s                    (Comb p as) = Comb p (mapApply s as)

mapApply :: Subst -> [Term] -> [Term]
mapApply s ts = (map (apply s) ts)

compose :: Subst -> Subst -> Subst
--compose s2@(Subst ss2 vs2) (Subst ss1 vs1) = Subst ((apply s2) . ss1) (vs1++vs2)
compose (Subst s1) (Subst s0) = let mayApp = \(v,_) -> not . (elem v) . fst . unzip $ s0
                                    app = filter mayApp s1
                                in Subst (s0 ++ app)

testCompose :: Subst -> Subst -> Term -> Bool
testCompose s2 s1 t = apply (compose s2 s1) t == apply s2 (apply s1 t)