module Pretty where
-- Module for Task 1

import Data.List
import Data.Char
import Type
import Subst
import Unification
import SLD

-- Pretty Class for Prolog syntax output
class Pretty a where
  
  prettyWithVarsFun :: (VarIndex -> String) -> a -> String
  
  prettyWithVars :: [(VarIndex, String)] -> a -> String
  prettyWithVars vss = prettyWithVarsFun (fun vss) 
    where
      fun :: [(VarIndex, String)] -> VarIndex -> String
      fun []           vi             = intToAlphabet vi
      fun ((v,s):next) vi | v == vi   = s
                          | otherwise = fun next vi
  pretty :: a -> String
  pretty a = prettyWithVarsFun intToAlphabet a
    
-- Converts a VarIndex to its corosponding letter(s)
intToAlphabet :: VarIndex -> String
intToAlphabet n =
  let prev  = n `div` 26
      digit = (chr (ord 'A' + (n `mod` 26))) : []
  in if prev == 0 then digit
                  else (intToAlphabet (prev - 1)) ++ digit


-- Pretty Instance for Term (Task 1)
instance Pretty Term where
  --prettyWithVarsFun :: (VarIndex -> String) -> a -> String
  prettyWithVarsFun f  (Var vi)                    = f vi
  prettyWithVarsFun f  (Comb p args) | args == []  = p
                                       | p == "."  = '[' : joinList args ++ "]"
                                       | otherwise = p ++ '(' : (intercalate "," (map (prettyWithVarsFun f) args)) ++ ")"
    where 
      -- Builds the prolog list presentation
      joinList :: [Term] -> String
      joinList (e:ls:[]) =
        (prettyWithVarsFun f) e ++ (case ls of
                                         Var _        -> '|' : (prettyWithVarsFun f) ls
                                         Comb "." lss -> ", " ++ joinList lss
                                         Comb "[]" _  -> ""
                                         _            -> error "invalid list format")
      joinList _         = 
        error "invalid list format 2"


instance Pretty Subst where
  prettyWithVarsFun f (Subst s) =
    '{' : (intercalate ", " (map (uncurry singlePretty) s)) ++ "}"
    where
      singlePretty :: VarIndex -> Term -> String
      singlePretty v t =
        (prettyWithVarsFun f) (Var v) ++ " -> " ++ (prettyWithVarsFun f) t
        
        
        
--TESTING STUFF:
instance Pretty SLDTree where
  prettyWithVarsFun f t = pretty' t ""
    where
      pretty' :: SLDTree -> String -> String
      pretty' (Node (Goal ts) edges) pre = 
        unlines ((pre++(intercalate ", " (map (prettyWithVarsFun f) ts))++"."):(concatMap (prettyE ('\t':pre)) edges))
        
      prettyE :: String -> (Subst, SLDTree) -> [String]
      prettyE pre2 (subst, tree) = [pre2++((prettyWithVarsFun f) subst), (pretty' tree pre2)]


instance Pretty Prog where
  prettyWithVarsFun fun (Prog rs) = unlines ( map (prettyWithVarsFun fun) rs)
instance Pretty Rule where
  prettyWithVarsFun fun (t:-ts) = (prettyWithVarsFun fun) t ++ " :- " ++ (intercalate ", " $ map (prettyWithVarsFun fun) ts)
instance Pretty Goal where
  prettyWithVarsFun fun (Goal ts) = (intercalate ", " $ map (prettyWithVarsFun fun) ts)

printprogtest = putStrLn.pretty $ progtest
printgoaltest = putStrLn.pretty $ goaltest
printttree = putStrLn.pretty $ ttree
