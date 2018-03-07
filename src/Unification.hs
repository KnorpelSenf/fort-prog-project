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
unify t0 t1 = unify' empty
  where
    unify' :: Subst -> Maybe Subst
    unify' s = makeSubst s
                $ uncurry ds
                $ mapTuple (apply s)
                $ (t0, t1)

    mapTuple :: (a -> b) -> (a, a) -> (b, b)
    mapTuple f (a1, a2) = (f a1, f a2)

    makeSubst :: Subst -> Maybe (Term, Term) -> Maybe Subst
    makeSubst sub mtt = case mtt of
                             Nothing          -> Just sub
                             Just (Var vi, t) -> if varNotIn vi t 
                                                 then unify' (compose (single vi t) sub) 
                                                 else Nothing
                             Just (t, Var vi) -> makeSubst sub (Just (Var vi, t))
                             _                -> Nothing

    varNotIn :: VarIndex -> Term -> Bool
    varNotIn vi (Var vii)     = vi /= vii
    varNotIn vi (Comb p args) = all (varNotIn vi) args

    {-
--Testing:
tfg0 = (Comb "f" [Comb "g" [Var 0]])
tfg1 = (Comb "f" [Comb "g" [Comb "blubb" [] ]])
tf0 = (Comb "f" [Comb "x" [], Var 0, Comb "true" []])
tf1 = (Comb "f" [Comb "x" [], Comb "ARGGHH!" [], Var 1])
tg0 = Comb "g" [Comb "x" [], Comb "x" [], Comb "x" []]
tv0 = Var 0
-}