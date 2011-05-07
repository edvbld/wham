module Wham.Interpreter (
       step,
       StateMode(..), 
       Configuration, 
       Stack, 
       toState,
       StackElement(..)) where

import Prelude hiding ((+), (-), (*), (/), (<=), (==), (&&))
import qualified Data.Map as Map
import Wham.AMDefinitions

data StateMode = Normal 
               | Exception 
                 deriving (Eq, Show)

data StackElement a b = StackInteger a
                      | StackBool b
                      deriving (Show)
type Stack a b = [StackElement a b]
type State a = Map.Map String a
type Result a b = Either String (Configuration a b)
type Configuration a b = ([AMExpression], Stack a b, (State a, StateMode))

step :: (AMNum a b, AMBoolean b) => Configuration a b -> Result a b
step (exps, stack, state) = istep exps stack state

istep :: (AMNum a b, AMBoolean b) => [AMExpression] -> Stack a b -> 
         (State a, StateMode) -> Result a b
istep [] [] state = Right ([], [], state)
istep (PUSH n:exps) stack state@(_, Normal) = 
    Right (exps, (StackInteger (absInteger n)):stack, state)
istep (STORE x:exps) (StackInteger n:stack) (state, Normal) = 
    if isBottom n then Right (exps, stack, (state, Exception))
                  else Right (exps, stack, (state', Normal))
        where state' = update x n state 
istep (FETCH x:exps) stack s@(state, Normal) = 
    case Map.lookup x state of
        Just value -> Right (exps, (StackInteger value):stack, s)
        Nothing -> Left $ "Could not find value for variable " ++ x
istep (NOOP:exps) stack state@(_, Normal) = 
    Right (exps, stack, state)
istep (ADD:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) = 
    Right (exps, stack', state)
        where stack' = StackInteger (a + b):stack
istep (SUB:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) = 
    Right (exps, stack', state)
        where stack' = StackInteger (a - b):stack
istep (MULT:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) = 
    Right (exps, stack', state)
        where stack' = StackInteger (a * b):stack
istep (DIV:exps) (StackInteger a:StackInteger b:stack) (state, Normal) = 
    Right (exps, stack', (state, Normal))
        where stack' = StackInteger (a / b):stack
istep (TRUE:exps) stack state@(_, Normal) = 
    Right (exps, StackBool (absBool True):stack, state)
istep (FALSE:exps) stack state@(_, Normal) = 
    Right (exps, StackBool (absBool False):stack, state)
istep (EQUAL:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = StackBool (a == b):stack
istep (LE:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = StackBool (a <= b):stack
istep (AND:exps) (StackBool a:StackBool b:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = StackBool (a && b):stack
istep (NEG:exps) (StackBool a:stack) state@(_, Normal) =
    Right (exps, stack', state)
        where stack' = StackBool (neg a):stack
istep (BRANCH s1 s2:exps) (StackBool b:stack) (state, Normal) =
    if isBottom b then Right (exps, stack, (state, Exception))
                  else Right (exps', stack, (state, Normal))
        where exps' = (cond b s1 s2) ++ exps
istep (LOOP b s:exps) stack state@(_, Normal) =
    Right (exps', stack, state)
        where exps' = b ++ [(BRANCH (s ++ [LOOP b s]) [NOOP])] ++ exps
istep (TRY s1 s2:exps) stack state@(_, Normal) =
    Right (s1 ++ (CATCH s2):exps, stack, state)
istep (CATCH _:exps) stack state@(_, Normal) =
    Right (exps, stack, state)
istep (CATCH s:exps) stack (state, Exception) =
    Right ((s ++ exps), stack, (state, Normal))
istep (_:exps) stack state@(_, Exception) =
    Right (exps, stack, state)
istep exps stack state = Left $ "Encountered bad configuration: \n" ++
                                "\tCode\n\t:" ++ (show exps) ++
                                "\tStack\n\t" ++ (show stack) ++
                                "\tState\n\t" ++ (show state)

update :: (AMNum a b) => String -> a -> State a -> State a
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s

toState :: (AMNum a b) => [(String, a)] -> (State a, StateMode)
toState list = (Map.fromList list, Normal)
