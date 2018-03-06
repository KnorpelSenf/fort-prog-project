module SLD where
-- Module for Task 4

import Type
import Subst
import Unification
import Data.List
import Data.Maybe

-- Datatype for SLD Trees
data SLDTree = Node Goal [(Subst, SLDTree)]

sld :: Prog -> Goal -> SLDTree
sld _                    g@(Goal [])
  = Node g []
sld prog@(Prog progRulz) goal@(Goal (gt:erms))
  = Node goal $ map    (\(subst, ts)  -> (subst, sld prog
                                                   $ Goal
                                                   $ mapApply subst ts))
              $ map    (\(Just s, rb) -> (s, rb ++ erms))
              $ filter (not . isNothing . fst)
              $ map    (\(rh :- rb)   -> (unify gt rh, rb))
              $ map    (uniqueVars goal)
              $ progRulz

uniqueVars :: Goal -> Rule -> Rule
uniqueVars (Goal goalTerms) rule@(rh :- rb) = flip mapRuleVars rule
                                                $ zip (concatMap varsIn
                                                         $ rh : rb)
                                                $ ([0..] \\)
                                                $ concatMap varsIn
                                                $ goalTerms
                                           
mapRuleVars :: [(VarIndex, VarIndex)] -> Rule -> Rule
mapRuleVars m (rh :- rb) = (mapTermVars m rh) :- (map (mapTermVars m) rb)

mapTermVars :: [(VarIndex, VarIndex)] -> Term -> Term
mapTermVars []         t             = t
mapTermVars ((d,z):rm) t@(Var v)     = if d == v then Var z else mapTermVars rm t
mapTermVars m          t@(Comb s ts) = Comb s (map (mapTermVars m) ts)

varsIn :: Term -> [VarIndex]
varsIn t = varsIn' t []
  where varsIn' :: Term -> [VarIndex] -> [VarIndex]
        varsIn' (Var vi)    vis = vi:vis
        varsIn' (Comb _ ts) vis = (concatMap varsIn ts) ++ vis


{-
    -- alle vars aus rule holen
    -- [0..] \\ vars aus rule sind die benutzten vars
    -- zip vars aus goal mit den benutzen liefert ein mapping
    -- alles in der regel mappen
    
    
    replaceInRule :: Rule -> [VarIndex] -> Rule
    replaceInRule (rh :- rb) vs = let rep = replaceInTerm rh vs
                                  in (fst rep) :- (replaceInTerms rb $ snd rep)
    
    replaceInTerms :: [Term] -> [VarIndex] -> ([Term], [VarIndex])
    replaceInTerms []     vs = ([], vs)
    replaceInTerms (t:ts) vs = let rep = replaceInTerm t vs
                               in (fst rep):(replaceInTerms ts $ snd rep)
    
    replaceInTerm :: Term -> [VarIndex] -> (Term, [VarIndex])
    replaceInTerm (Var _)     vs = (Var (head vs), tail vs)
    replaceInTerm (Comb s ts) vs = let rep = replaceInTerms ts vs
                                    in (Comb s (fst rep), snd rep)
-}

      
--TESTING
s1 = single 1 (Var 2)
s2 = single 0 (Comb "f" [Var 1, Comb "true" []])
cs  = compose s1 s2

tt :: SLDTree
tt = Node (Goal [Var 1]) [(s1, Node (Goal [Var 123]) []), (s2, Node (Goal [Var 234]) [])]

progtest = Prog [r0, r1, r2]
-- p(A,B) :- q(A,C), p(C,B).
r0  = r0h:- [Comb "q" [Var 0, Var 2], Comb "p" [Var 2, Var 1]]
r0h = Comb "p" [Var 0, Var 1]
-- p(D,D).
r1  = r1h:-[]
r1h = Comb "p" [Var 0, Var 0]
-- q(a,b).
r2  = r2h:-[]
r2h = Comb "q" [Comb "a" [], Comb "b" []]

goaltest = Goal [t0]
-- p(E,b).
t0 = Comb "p" [Var 0, Comb "b" []]

pt0 = Prog [(Comb "a" [Comb "x" [], Comb "y" []]) :- []]
-- a(x,y).
gt0 = Goal [Comb "a" [Var 1, Var 0]]
-- -? a(B,A).

ttree = sld progtest goaltest
