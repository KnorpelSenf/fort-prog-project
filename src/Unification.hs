module Unification where 
-- Module for Task 3 

import Type
import Subst
import Data.Maybe

-- Computes the disagreement set
ds :: Term -> Term -> Maybe (Term, Term)
ds t0@(Var vi)     t1              | isNotThisVar vi t1                 = Just (t0, t1)
ds t0              t1@(Var vi)     | isNotThisVar vi t0                 = Just (t0, t1)
ds t0@(Comb p0 a0) t1@(Comb p1 a1) | p0 == p1 && length a0 == length a1 = listToMaybe
                                                                          $ catMaybes
                                                                          $ map (uncurry ds)
                                                                          $ zip a0 a1
                                   | otherwise                          = Just (t0, t1)
--  where
isNotThisVar :: VarIndex -> Term -> Bool
isNotThisVar vi (Comb _ _) = True
isNotThisVar vi (Var v)    = vi == v


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
