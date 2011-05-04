module Interpreter(evaluate,
                   step,
                   toState,
                   StateMode(..), 
                   Configuration, 
                   Stack, 
                   EState,
                   StackElement(..)) where

import qualified Data.Map as Map
import AMDefinitions

data StackElement = Integer Integer
                  | Bool Bool
                  | Bottom
                  deriving (Show)
type Stack = [StackElement]
data StateMode = Normal 
               | Exception 
                 deriving (Eq, Ord, Show)
type State = Map.Map String Integer
type EState = (State, StateMode)
type Configuration = ([AMExpression], Stack, EState)

evaluate :: Configuration -> Either String Configuration
evaluate ([], stack, state) = Right ([], stack, state)
evaluate c = case step c of 
                Right conf -> evaluate conf
                Left err -> Left err

step :: Configuration -> Either String Configuration
step (exps, stack, state) = istep exps stack state

istep :: [AMExpression] -> Stack -> EState -> Either String Configuration
istep [] stack state = Right ([], stack, state)
istep (PUSH n:exps) stack state@(_, Normal) = 
    Right (exps, Integer n:stack, state)
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
    if b == 0
        then Right (exps, Bottom:stack, (state, Exception))
        else Right (exps, stack', (state, Normal))
        where
            stack' = Integer (div a b):stack
istep (TRUE:exps) stack state@(_, Normal) = 
    Right (exps, Bool True:stack, state)
istep (FALSE:exps) stack state@(_, Normal) = 
    Right (exps, Bool False:stack, state)
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
        where stack' = Bool (not a):stack
istep (BRANCH s1 s2:exps) (Bool b:stack) state@(_, Normal) =
    Right (exps', stack, state)
        where exps' = if b then s1 ++ exps else s2 ++ exps
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

toState :: [(String, Integer)] -> EState
toState list = (Map.fromList list, Normal)

update :: String -> Integer -> State -> State
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s
