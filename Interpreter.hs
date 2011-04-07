module Interpreter(evaluate) where

import qualified Data.Map as Map
import Translator

data StackElement = Integer Integer | Bool Bool 
type Stack = [StackElement]
type AMState = Map.Map String Integer

evaluate :: [AMExpression] -> [(String, Integer)] -> [(String, Integer)]
evaluate exps state = Map.toList (eval exps [] (Map.fromList state))

eval :: [AMExpression] -> Stack -> AMState -> AMState
eval [] stack state = state
eval ((PUSH n):exps) stack state = eval exps ((Integer n):stack) state
eval ((STORE x):exps) ((Integer n):stack) state = eval exps stack state'
    where state' = update x n state 
eval ((FETCH x):exps) stack state = eval exps stack' state
    where stack' = ((Interpreter.lookup x state):stack) 
eval (NOOP:exps) stack state = eval exps stack state
eval (ADD:exps) ((Integer a):(Integer b):stack) state = eval exps stack' state
    where stack' = ((Integer (a+b)):stack)
eval (SUB:exps) ((Integer a):(Integer b):stack) state = eval exps stack' state
    where stack' = ((Integer (a-b)):stack)
eval (MULT:exps) ((Integer a):(Integer b):stack) state = eval exps stack' state
    where stack' = ((Integer (a*b)):stack)
eval (DIV:exps) ((Integer a):(Integer b):stack) state = eval exps stack' state
    where stack' = ((Integer (div a b)):stack)
eval (TRUE:exps) stack state = eval exps ((Bool True):stack) state
eval (FALSE:exps) stack state = eval exps ((Bool False):stack) state
eval (EQUAL:exps) ((Integer a):(Integer b):stack) state = eval exps stack' state
    where stack' = ((Bool (a == b)):stack)
eval (LE:exps) ((Integer a):(Integer b):stack) state = eval exps stack' state
    where stack' = ((Bool (a <= b)):stack)
eval (AND:exps) ((Bool a):(Bool b):stack) state = eval exps stack' state
    where stack' = ((Bool (a && b)):stack)
eval (NEG:exps) ((Bool a):stack) state = eval exps stack' state
    where stack' = ((Bool (not a)):stack)
eval ((BRANCH s1 s2):exps) ((Bool b):stack) state = eval exps' stack state
    where exps' = if b then s1 ++ exps else s2 ++ exps
eval ((LOOP b s):exps) stack state = eval exps' stack state
    where exps' = b ++ [(BRANCH (s ++ [LOOP b s]) [NOOP])] ++ exps

update :: String -> Integer -> AMState -> AMState
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s

lookup :: String -> AMState -> StackElement
lookup x s = case Map.lookup x s of
                Just n -> (Integer n)
