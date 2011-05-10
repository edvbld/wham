module Wham.Analyzer(analyze) where

import Wham.SignExc
import Wham.SignBoolExc
import Wham.Interpreter
import Wham.AMDefinitions hiding ((==), (+))
import qualified Data.Set as Set
import qualified Data.Map as Map

type ControlPointMap = Map.Map Integer (State SignExc)
type ConfigurationSet = Set.Set (Configuration SignExc SignBoolExc)
type ConfigurationQueue = Map.Map Integer [Configuration SignExc SignBoolExc]

analyze :: [AMExpression] -> [(String, Integer)] -> 
           Either String ControlPointMap
analyze code st = run (Map.singleton 0 [(code, [], toState state')]) Map.empty
    where state' = map (\(s,i) -> (s, absInteger $ Just i)) st

run :: ConfigurationQueue -> ControlPointMap -> 
       Either String ControlPointMap
run queue cps 
    | empty queue = Right cps
    | otherwise = case astep queue of
                     Right (states, queue') -> run queue' (cps' states)
                     Left err -> Left err
        where
            cp = cpToExec queue
            cps' states = foldl (\m s -> Map.insert cp s m) cps states

empty :: ConfigurationQueue -> Bool
empty q = Map.null q

astep :: ConfigurationQueue -> 
         Either String ([State SignExc], ConfigurationQueue)
astep queue = case istep queue of 
    Right (queue', set) -> Right (finished set, increase queue' set)
    Left err -> Left err
    where 
        finished set = map state $ filter done $ Set.toList set
        increase q set = foldl (\acc c -> update acc c) q $ Set.toList set

update :: ConfigurationQueue -> Configuration SignExc SignBoolExc -> 
          ConfigurationQueue
update q c = 
    case mcp of
        Nothing -> q
        Just cp -> case Map.lookup cp q of 
                    Just confs -> Map.insert cp (c:confs) q
                    Nothing -> Map.insert cp [c] q
    where mcp = getCP c

getCP :: Configuration SignExc SignBoolExc -> Maybe Integer
getCP ([], _, _) = Nothing
getCP ((PUSH _ cp):_, _, _) = Just cp
getCP ((FETCH _ cp):_, _, _) = Just cp
getCP ((STORE _ cp):_, _, _) = Just cp
getCP ((BRANCH _ _ cp):_, _, _) = Just cp
getCP ((LOOP _ _ cp):_, _, _) = Just cp
getCP ((TRY _ _ cp):_, _, _) = Just cp
getCP ((CATCH _ cp):_, _, _) = Just cp
getCP ((NOOP cp):_, _, _) = Just cp
getCP ((TRUE cp):_, _, _) = Just cp
getCP ((FALSE cp):_, _, _) = Just cp
getCP ((ADD cp):_, _, _) = Just cp
getCP ((SUB cp):_, _, _) = Just cp
getCP ((MULT cp):_, _, _) = Just cp
getCP ((DIV cp):_, _, _) = Just cp
getCP ((NEG cp):_, _, _) = Just cp
getCP ((EQUAL cp):_, _, _) = Just cp
getCP ((LE cp):_, _, _) = Just cp
getCP ((AND cp):_, _, _) = Just cp

state :: Configuration SignExc SignBoolExc -> State SignExc
state (_,_, (s,_)) = s

done :: Configuration SignExc SignBoolExc -> Bool
done ([], [], _) = True
done _ = False

cpToExec :: ConfigurationQueue -> Integer
cpToExec q = cp
    where
        keys = Map.keys q
        cp = foldl min (head keys) keys

istep :: ConfigurationQueue -> 
         Either String (ConfigurationQueue, ConfigurationSet)
istep queue = case Map.lookup cp queue of 
    Just conf -> case step $ head conf of
                    Right confs -> Right (adjust cp queue, confs)
                    Left err -> Left err
    Nothing -> Left $ "Could not find any configuratuion for control point " ++
                      (show cp)
    where
        cp = cpToExec queue

adjust :: Integer -> ConfigurationQueue -> ConfigurationQueue
adjust cp q = case Map.lookup cp q of
    Just conf -> if length conf == 1
                    then Map.delete cp q
                    else Map.insert cp (tail conf) q
    Nothing -> q
