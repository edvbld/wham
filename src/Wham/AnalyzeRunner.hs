module Wham.AnalyzeRunner(
    run, 
    ControlPointMap, 
    ConfigurationSet) where

import Wham.SignExc
import Wham.SignBoolExc
import Wham.InterpreterTypes
import Wham.Interpreter
import Wham.AMDefinitions hiding ((==), (+))
import qualified Data.Set as Set
import qualified Data.Map as Map

type AbstractConfiguration = (Configuration SignExc SignBoolExc)
type ConfigurationSet = Set.Set AbstractConfiguration
type ControlPointMap = 
    Map.Map Integer ConfigurationSet
type ConfigurationQueue = Map.Map Integer [AbstractConfiguration]

run :: ConfigurationQueue -> ControlPointMap -> 
       Either String ControlPointMap
run queue cpm 
    | empty queue = Right cpm
    | otherwise = case astep queue cpm of
                     Right (confs, queue') -> run queue' (cpm' confs)
                     Left err -> Left err
        where
            cp = cpToExec queue
            Just cs = Map.lookup cp queue
            c = head cs
            cpm' confs = foldl (insert c) cpm confs

insert :: AbstractConfiguration -> ControlPointMap -> 
          AbstractConfiguration -> ControlPointMap
insert _ m c@((STORE _ cp):_, _, _) = insertImpl cp c m
insert _ m c@((BRANCH _ _ cp):_,_,_) = insertImpl cp c m
insert _ m c@((TRY _ _ cp):_, _, _) = insertImpl cp c m
insert ((ANALYZE  cp):_, _, _) m c = insertImpl cp c m
insert _ m c@((NOOP cp):_, _, _) = insertImpl cp c m 
insert _ m _ = m

insertImpl :: Integer -> AbstractConfiguration -> ControlPointMap ->
              ControlPointMap
insertImpl cp c m = case Map.lookup cp m of
                     Nothing -> Map.insert cp (Set.singleton c) m
                     Just set -> Map.insert cp (Set.insert c set) m

empty :: ConfigurationQueue -> Bool
empty q = Map.null q

astep :: ConfigurationQueue -> ControlPointMap ->
         Either String ([AbstractConfiguration], ConfigurationQueue)
astep queue cps = case istep queue of 
    Right (queue', set) -> Right (Set.toList set, increase queue' set)
    Left err -> Left err
    where 
        increase q set = foldl (\acc c -> update acc c cps) q $ Set.toList set

update :: ConfigurationQueue -> AbstractConfiguration -> 
          ControlPointMap -> ConfigurationQueue
update q c cps = 
    case mcp of
        Nothing -> q
        Just cp -> case Map.lookup cp cps of
                    Just set -> case Set.member c set of
                                 True -> q
                                 False -> q' cp
                    Nothing -> q' cp
   where
    mcp = getCP c
    q' cp = case Map.lookup cp q of 
            Just confs -> Map.insert cp (c:confs) q
            Nothing -> Map.insert cp [c] q

getCP :: AbstractConfiguration -> Maybe Integer
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
getCP ((ANALYZE cp):_, _, _) = Just cp

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
    Just confs -> if length confs == 1
                    then Map.delete cp q
                    else Map.insert cp (tail confs) q
    Nothing -> q
