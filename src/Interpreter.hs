module Interpreter (
       evaluate,
       step,
       toState,
       StateMode(..), 
       Configuration, 
       Stack, 
       StackElement(..)) where

import Prelude hiding ((+), (-), (*), (/), (<=), (==), (&&))
import qualified Data.Map as Map
import AMDefinitions

data StackElement a b = Integer a
                      | Bool b
                      | Bottom
                      deriving (Show)
type Stack a b = [StackElement a b]
data StateMode = Normal 
               | Exception 
                 deriving (Eq, Ord, Show)
type State a = Map.Map String a
type Result a b = Either String (Configuration a b)
type Configuration a b = ([AMExpression], Stack a b, (State a, StateMode))

evaluate :: (AMNum a b, AMBoolean b) => Configuration a b -> Result a b
evaluate ([], stack, state) = Right ([], stack, state)
evaluate c = case step c of 
                Right conf -> evaluate conf
                Left err -> Left err

step :: (AMNum a b, AMBoolean b) => Configuration a b -> Result a b
step (exps, stack, state) = istep exps stack state

istep :: (AMNum a b, AMBoolean b) => [AMExpression] -> Stack a b -> 
         (State a, StateMode) -> Result a b
istep [] stack state = Right ([], stack, state)
istep (PUSH n:exps) stack state@(_, Normal) = 
    Right (exps, (Integer (absInteger n)):stack, state)
istep (STORE x:exps) (Integer n:stack) (state, Normal) = 
    Right (exps, stack, (state', Normal))
        where state' = update x n state 
istep (FETCH x:exps) stack s@(state, Normal) = 
    case Map.lookup x state of
        Just value -> Right (exps, (Integer value):stack, s)
        Nothing -> Left $ "Could not find value for variable " ++ x
istep (NOOP:exps) stack state@(_, Normal) = 
    Right (exps, stack, state)
istep (ADD:exps) (Integer a:Integer b:stack) state@(_, Normal) = 
    Right (exps, stack', state)
        where stack' = Integer (a+b):stack
istep (SUB:exps) (Integer a:Integer b:stack) state@(_, Normal) = 
    Right (exps, stack', state)
        where stack' = Integer (a-b):stack
istep (MULT:exps) (Integer a:Integer b:stack) state@(_, Normal) = 
    Right (exps, stack', state)
        where stack' = Integer (a*b):stack
istep (DIV:exps) (Integer a:Integer b:stack) (state, Normal) = 
    if toBool $ b == (absInteger 0)
        then Right (exps, Bottom:stack, (state, Exception))
        else Right (exps, stack', (state, Normal))
        where
            stack' = Integer (a / b):stack
istep (TRUE:exps) stack state@(_, Normal) = 
    Right (exps, Bool (absBool True):stack, state)
istep (FALSE:exps) stack state@(_, Normal) = 
    Right (exps, Bool (absBool False):stack, state)
istep (EQUAL:exps) (Integer a:Integer b:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = Bool (a == b):stack
istep (LE:exps) (Integer a:Integer b:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = Bool (a <= b):stack
istep (AND:exps) (Bool a:Bool b:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = Bool (a && b):stack
istep (NEG:exps) (Bool a:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = Bool (neg a):stack
istep (BRANCH s1 s2:exps) (Bool b:stack) state@(_, Normal) =
    Right (exps', stack, state)
        where exps' = if (toBool b) then s1 ++ exps else s2 ++ exps
istep (LOOP b s:exps) stack state@(_, Normal) =
    Right (exps', stack, state)
        where exps' = b ++ [(BRANCH (s ++ [LOOP b s]) [NOOP])] ++ exps
istep (TRY s1 s2:exps) stack state@(_, Normal) =
    Right (s1 ++ (CATCH s2):exps, stack, state)
istep (CATCH _:exps) stack state@(_, Normal) =
    Right (exps, stack, state)
istep (CATCH s:exps) (Bottom:stack) (state, Exception) =
    Right ((s ++ exps), stack, (state, Normal))
istep (_:exps) stack state@(_, Exception) =
    Right (exps, stack, state)
istep exps stack state = Left $ "Encountered bad configuration: \n" ++
                                "\tCode\n\t:" ++ (show exps) ++
                                "\tStack\n\t" ++ (show stack) ++
                                "\tState\n\t" ++ (show state)

toState :: (AMNum a b) => [(String, a)] -> (State a, StateMode)
toState list = (Map.fromList list, Normal)

update :: (AMNum a b) => String -> a -> State a -> State a
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s
