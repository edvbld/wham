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
eval ((ADD):exps) ((Integer a):(Integer b):stack) state = eval exps stack' state
    where stack' = ((Integer (a+b)):stack)
eval ((STORE x):exps) ((Integer n):stack) state = eval exps stack state'
    where state' = update x n state

update :: String -> Integer -> AMState -> AMState
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s
