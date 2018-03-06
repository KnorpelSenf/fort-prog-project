module Unification where 
-- Module for Task 3 

import Type
import Subst
import Data.Maybe

-- Computes the disagreement set
ds :: Term -> Term -> Maybe (Term, Term)
ds t0              t1             | t0 == t1  = Nothing
ds t0@(Var _)      t1             | t0 /= t1  = Just (t0, t1)
ds t0              t1@(Var _)     | t0 /= t1  = Just (t0, t1)
ds t0@(Comb p0 a0) t1@(Comb p1 a1)            = if p0 == p1 && length a0 == length a1
                                                   then listToMaybe (filter (uncurry (/=)) (zip a0 a1))
                                                   else Just (t0, t1)
ds _               _                          = error "ds calc error"

-- Computes the most general unifier
unify :: Term -> Term -> Maybe Subst
unify t0 t1 = unify' t0 t1 empty
  where
    unify' :: Term -> Term -> Subst -> Maybe Subst
    unify' t0 t1 s = case ds (apply s t0) (apply s t1) of
                          Nothing          -> Just s
                          Just (Var vi, t) -> if varNotIn vi t 
                                                 then unify' t0 t1 (compose (single vi t) s) 
                                                 else Nothing
                          Just (t, Var vi) -> if varNotIn vi t 
                                                 then unify' t0 t1 (compose (single vi t) s) 
                                                 else Nothing
                          _                -> Nothing

    varNotIn :: VarIndex -> Term -> Bool
    varNotIn vi (Var vii)     = vi /= vii
    varNotIn vi (Comb p args) = all (varNotIn vi) args

--Testing:
-- tf0 = (Comb "f" [Comb "x" [], Var 0, Comb "true" []])
-- tf1 = (Comb "f" [Comb "x" [], Comb "ARGGHH!" [], Var 1])
-- tg0 = Comb "g" [Comb "x" [], Comb "x" [], Comb "x" []]
-- tv0 = Var 0
