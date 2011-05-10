module Wham.Analyzer(analyze) where

import Wham.SignExc
import Wham.SignBoolExc
import Wham.Interpreter
import Wham.AMDefinitions hiding ((==), (+))
import qualified Data.Set as Set
import qualified Data.Map as Map

type ControlPointMap = Map.Map Integer (State SignExc)

analyze :: [AMExpression] -> [(String, Integer)] -> 
           Either String ControlPointMap
analyze code state = run (code, [], toState state') Map.empty
    where state' = map (\(s,i) -> (s, absInteger $ Just i)) state

run :: Configuration SignExc SignBoolExc -> ControlPointMap -> 
       Either String ControlPointMap 
run ([], [], (state, _)) cps = Right cps'
    where
        keys = Map.keys cps
        maxCP = foldl max (head keys) keys
        cps' = Map.insert (maxCP + 1) state cps
run conf@((STORE _ cp:_), _, (state, _)) cps = 
    case step conf of 
        Right confs -> run (head (Set.elems confs)) cps'
        Left err -> Left err
    where cps' = Map.insert cp state cps
run conf cps = 
    case step conf of 
        Right confs -> run (head (Set.elems confs)) cps
        Left err -> Left err
