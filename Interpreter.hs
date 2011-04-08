module Interpreter(evaluate, StateMode(..)) where

import qualified Data.Map as Map
import Translator

data StackElement = Integer Integer | Bool Bool 
type Stack = [StackElement]
data StateMode = Normal 
               | Exception 
                 deriving (Eq, Ord, Show)
type State = Map.Map String Integer
type EState = (State, StateMode)

evaluate :: [AMExpression] -> [(String, Integer)] -> 
            ([(String, Integer)], StateMode)
evaluate exps state = (stateList, mode)
    where
        (finalState, mode) = eval exps [] ((Map.fromList state), Normal)
        stateList = Map.toList finalState
 
eval :: [AMExpression] -> Stack -> EState -> EState
eval [] stack state = state
eval ((PUSH n):exps) stack (state, Normal) = 
    eval exps ((Integer n):stack) (state, Normal)
eval ((STORE x):exps) ((Integer n):stack) (state, Normal) = 
    eval exps stack (state', Normal)
        where state' = update x n state 
eval ((FETCH x):exps) stack (state, Normal) = eval exps stack' (state, Normal)
    where stack' = ((Interpreter.lookup x state):stack) 
eval (NOOP:exps) stack (state, Normal) = eval exps stack (state, Normal)
eval (ADD:exps) ((Integer a):(Integer b):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Integer (a+b)):stack)
eval (SUB:exps) ((Integer a):(Integer b):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Integer (a-b)):stack)
eval (MULT:exps) ((Integer a):(Integer b):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Integer (a*b)):stack)
eval (DIV:exps) ((Integer a):(Integer b):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Integer (div a b)):stack)
eval (TRUE:exps) stack (state, Normal) = 
    eval exps ((Bool True):stack) (state, Normal)
eval (FALSE:exps) stack (state, Normal) = 
    eval exps ((Bool False):stack) (state, Normal)
eval (EQUAL:exps) ((Integer a):(Integer b):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Bool (a == b)):stack)
eval (LE:exps) ((Integer a):(Integer b):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Bool (a <= b)):stack)
eval (AND:exps) ((Bool a):(Bool b):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Bool (a && b)):stack)
eval (NEG:exps) ((Bool a):stack) (state, Normal) = 
    eval exps stack' (state, Normal)
        where stack' = ((Bool (not a)):stack)
eval ((BRANCH s1 s2):exps) ((Bool b):stack) (state, Normal) = 
    eval exps' stack (state, Normal)
        where exps' = if b then s1 ++ exps else s2 ++ exps
eval ((LOOP b s):exps) stack (state, Normal) = 
    eval exps' stack (state, Normal)
        where exps' = b ++ [(BRANCH (s ++ [LOOP b s]) [NOOP])] ++ exps

update :: String -> Integer -> State -> State
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s

lookup :: String -> State -> StackElement
lookup x s = case Map.lookup x s of
                Just n -> (Integer n)
