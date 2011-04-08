module Interpreter(evaluate,
                   step,
                   StateMode(..), 
                   Configuration, 
                   Stack, 
                   EState,
                   StackElement(..)) where

import qualified Data.Map as Map
import Translator

data StackElement = Integer Integer 
                  | Bool Bool 
                    deriving (Show)
type Stack = [StackElement]
data StateMode = Normal 
               | Exception 
                 deriving (Eq, Ord, Show)
type State = Map.Map String Integer
type EState = (State, StateMode)
type Configuration = ([AMExpression], Stack, EState)

evaluate :: [AMExpression] -> [(String, Integer)] -> 
            ([(String, Integer)], StateMode)
evaluate exps variables = (stateList, mode)
    where
        state = (Map.fromList variables, Normal)
        (_, _, (finalState, mode)) = eval (exps, [], state)
        stateList = Map.toList finalState

eval :: Configuration -> Configuration
eval ([], stack, state) = ([], stack, state)
eval c = eval $ step c

step :: Configuration -> Configuration
step (exps, stack, state) = istep exps stack state

istep :: [AMExpression] -> Stack -> EState -> Configuration
istep [] stack state = ([], stack, state)
istep (PUSH n:exps) stack state@(_, Normal) = 
    (exps, Integer n:stack, state)
istep (STORE x:exps) (Integer n:stack) (state, Normal) = 
    (exps, stack, (state', Normal))
        where state' = update x n state 
istep (FETCH x:exps) stack s@(state, Normal) = 
    (exps, stack', s)
        where stack' = (Interpreter.lookup x state):stack
istep (NOOP:exps) stack state@(_, Normal) = 
    (exps, stack, state)
istep (ADD:exps) (Integer a:Integer b:stack) state@(_, Normal) = 
    (exps, stack', state)
        where stack' = Integer (a+b):stack
istep (SUB:exps) (Integer a:Integer b:stack) state@(_, Normal) = 
    (exps, stack', state)
        where stack' = Integer (a-b):stack
istep (MULT:exps) (Integer a:Integer b:stack) state@(_, Normal) = 
    (exps, stack', state)
        where stack' = Integer (a*b):stack
istep (DIV:exps) (Integer a:Integer b:stack) (state, Normal) = 
    if b == 0
        then (exps, stack, (state, Exception))
        else (exps, stack', (state, Normal))
        where
            stack' = Integer (div a b):stack
istep (TRUE:exps) stack state@(_, Normal) = 
    (exps, Bool True:stack, state)
istep (FALSE:exps) stack state@(_, Normal) = 
    (exps, Bool False:stack, state)
istep (EQUAL:exps) (Integer a:Integer b:stack) state@(_, Normal) =
    (exps, stack', state)
        where stack' = Bool (a == b):stack
istep (LE:exps) (Integer a:Integer b:stack) state@(_, Normal) =
    (exps, stack', state)
        where stack' = Bool (a <= b):stack
istep (AND:exps) (Bool a:Bool b:stack) state@(_, Normal) =
    (exps, stack', state)
        where stack' = Bool (a && b):stack
istep (NEG:exps) (Bool a:stack) state@(_, Normal) =
    (exps, stack', state)
        where stack' = Bool (not a):stack
istep (BRANCH s1 s2:exps) (Bool b:stack) state@(_, Normal) =
    (exps', stack, state)
        where exps' = if b then s1 ++ exps else s2 ++ exps
istep (LOOP b s:exps) stack state@(_, Normal) =
    (exps', stack, state)
        where exps' = b ++ [(BRANCH (s ++ [LOOP b s]) [NOOP])] ++ exps
istep (CATCH s:exps) stack state@(_, Normal) =
    (exps, stack, state)
istep (CATCH s:exps) stack (state, Exception) =
    ((s ++ exps), stack, (state, Normal))
istep (_:exps) stack state@(_, Exception) =
    (exps, stack, state)

update :: String -> Integer -> State -> State
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s

lookup :: String -> State -> StackElement
lookup x s = case Map.lookup x s of
                Just n -> (Integer n)
