module Wham.Evaluator (evaluate) where

import Wham.Interpreter
import Wham.AMDefinitions
import Wham.IntegerExc
import Wham.BoolExc
import qualified Data.Map as Map

evaluate :: [AMExpression] -> [(String, Integer)] -> 
            Either String ([(String, Integer)], StateMode)
evaluate code state = 
    case run (code, [], toState state') of 
        Right (_, _, (st, mode)) -> 
            Right (toList st, mode)
        Left err -> Left err
    where state' = map (\(s, i) -> (s, absInteger $ Just i)) state

toList :: (Map.Map String IntegerExc) -> [(String, Integer)]
toList m = convert (Map.toList m) []
    where
        convert [] acc = acc
        convert ((s, Integer i):t) acc = convert t ((s,i):acc)
        convert ((_, IntegerBottom):t) acc = convert t acc
    

run :: Configuration IntegerExc BoolExc -> 
       Either String (Configuration IntegerExc BoolExc)
run ([], [], state) = Right ([], [], state)
run c = case step c of 
            Right conf -> run conf
            Left err -> Left err

