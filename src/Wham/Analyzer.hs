module Wham.Analyzer(analyze) where

import Wham.SignExc
import Wham.SignBoolExc
import Wham.Interpreter
import Wham.AMDefinitions hiding ((==))
import qualified Data.Set as Set
import qualified Data.Map as Map

type StateAndMode = (Map.Map String SignExc, StateMode)
analyze :: [AMExpression] -> [(String, Integer)] -> 
           Either String StateAndMode
analyze code state = run (code, [], toState state')
    where state' = map (\(s,i) -> (s, absInteger $ Just i)) state

run :: Configuration SignExc SignBoolExc -> Either String StateAndMode 
run ([], [], state) = Right state
run conf = 
    case step conf of 
        Right confs -> if Set.size confs == 1
                         then 
                            run $ head $ Set.elems confs
                         else 
                            Left "Not implemented yet!"
        Left err -> Left err
