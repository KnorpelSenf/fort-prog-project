module Main where
-- Module for Task 6

import Parser
import Type
import Pretty
import Subst
import SLD
import Strategy

import System.IO
import System.Random
import Data.List

-- STATE
-- Datatype to store the current status of the interpreter
data State = State Strategy Bool Prog

startState :: State
startState = State dfs False (Prog [])

isQuit :: State -> Bool
isQuit (State _ q _) = q
-- /STATE


-- ACTIONS
type Action = State -> IO State
help ::           Action
help s = do putStrLn . unlines $ [
              "Commands available from the promt:",
              "  <goal>\t\tSolves the goal.",
              "  :help\t\tShows this help msg.",
              "  :info\t\tShows all available predicates.",
              "  :load <file>\t Loads the file.",
              "  :quit\t\tExits the UnLog environment.",
              "  :set <strat>\tSets the search strategy to dfs of bfs. (Preset to dfs)"]
            return s
            
info ::           Action
info s = let State st q (Prog rules) = s
             rs = map (\(rh:-rb) -> (pretty rh, length rb)) rules
         in do putStrLn . unlines $ 
                ("Available predicates: ":(map (\(p, l) -> p++"/"++(show l)) rs))
               return s

load :: String -> Action
load file (State s q p) = do par <- parseFile file
                             case par of
                                  Right np@(Prog _) -> do putStrLn "Loaded."
                                                          return (State s q np)
                                  Left str          -> do putStrLn str
                                                          return (State s q p)

quit ::           Action
quit (State s _ p) = do putStrLn "Good bye!"
                        return (State s True p)

set  :: String -> Action
set strat (State s q p) = if strat == "dfs" 
                             then do putStrLn "Set to dfs."
                                     return (State dfs q p) 
                             else if strat == "bfs" 
                              then do putStrLn "Set to bfs."
                                      return (State bfs q p)
                              else do putStrLn "No valid strategy with that name."
                                      return (State s q p)

goal :: String -> Action
goal qu st@(State s q p) = 
  let par = parseWithVars qu
  in case par of
          Left str                -> do putStrLn str
                                        return st
          Right (g@(Goal _), vss) -> let subs = solve s p g
                                     in do putSubst subs vss
                                           return st

putSubst :: [Subst] -> [(VarIndex, String)] -> IO ()
putSubst []        vss = do putStrLn "No more solutions."
                            return ()
putSubst (s:ubsts) vss = let o = prettyWithVars vss s
                             out = if o == "{}"
                                      then "True. "
                                      else o
                         in do
                              putStr (out)
                              hSetBuffering stdin NoBuffering
                              c <- getChar
                              hSetBuffering stdin LineBuffering
                              putStrLn ""
                              if c == 'n' || c == ';' 
                              then putSubst ubsts vss
                              else return ()

action :: String -> Action
action str | str == ":help" || str == ":h"            = help
           | str == ":info" || str == ":i"            = info
           | str == ":quit" || str == ":q"            = quit
           | (take 5 str) == ":set "                  = set (drop 5 str)
           | (take 3 str) == ":s "                    = set (drop 3 str)
           | (take 6 str) == ":load "                 = load (drop 6 str)
           | (take 3 str) == ":l "                    = load (drop 3 str)
           | otherwise                                = goal str

-- /ACTION


-- INTERACTION
main :: IO ()
main = do welcome
          unlogCycle startState

welcome :: IO ()
welcome = do rnd <- randomRIO (1::Int, 5::Int)
             msg <- if rnd == 1
                       then readFile "welcome-msg-shitlog.txt"
                       else readFile "welcome-msg-unlog.txt"
             putStrLn msg

unlogCycle :: State -> IO ()
unlogCycle state = do hSetBuffering stdout NoBuffering
                      hSetBuffering stdin LineBuffering
                      putStr "?- "
                      input <- getLine
                      newstate <- (action input) state
                      if isQuit newstate 
                        then return () 
                        else unlogCycle newstate
-- /INTERACTION
