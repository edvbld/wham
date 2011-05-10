module Wham.Interpreter (
       step,
       StateMode(..), 
       Configuration, 
       Stack, 
       toState,
       State,
       StackElement(..)) where

import Prelude hiding ((+), (-), (*), (/), (<=), (==), (&&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Wham.AMDefinitions

data StateMode = Normal 
               | Exception 
                 deriving (Eq, Show, Ord)

data StackElement a b = StackInteger a
                      | StackBool b
                      deriving (Show, Ord, Eq)
type Stack a b = [StackElement a b]
type State a = Map.Map String a
type Result a b = Either String (Set.Set (Configuration a b))
type Configuration a b = ([AMExpression], Stack a b, (State a, StateMode))

step :: (AMNum a, AMBoolean b) => Configuration a b -> Result a b
step (exps, stack, state) = istep exps stack state

ret :: (AMNum a, AMBoolean b) => Configuration a b -> Result a b
ret a = Right (Set.singleton a)

istep :: (AMNum a, AMBoolean b) => [AMExpression] -> Stack a b -> 
         (State a, StateMode) -> Result a b
istep [] [] state = ret ([], [], state)
istep (PUSH n _:exps) stack state@(_, Normal) = 
    ret (exps, (StackInteger $ absInteger $ Just n):stack, state)
istep (STORE x _:exps) (StackInteger n:stack) (state, Normal) = 
    case isBottom n of
        Yes -> ret (exps, stack, (state, Exception))
        Maybe -> Right set'
        No -> ret (exps, stack, (state', Normal))
    where
        state' = update x n state 
        state'' = update x (castToNonBottom n) state
        set = Set.singleton (exps, stack, (state, Exception))
        set' = Set.insert (exps, stack, (state'', Normal)) set
istep (FETCH x _:exps) stack s@(state, Normal) = 
    case Map.lookup x state of
        Just value -> ret (exps, (StackInteger value):stack, s)
        Nothing -> Left $ "Could not find value for variable " ++ x
istep (NOOP _:exps) stack state@(_, Normal) = 
    ret (exps, stack, state)
istep (ADD _:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) = 
    ret (exps, stack', state)
        where stack' = StackInteger (a + b):stack
istep (SUB _:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) = 
    ret (exps, stack', state)
        where stack' = StackInteger (a - b):stack
istep (MULT _:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) = 
    ret (exps, stack', state)
        where stack' = StackInteger (a * b):stack
istep (DIV _:exps) (StackInteger a:StackInteger b:stack) (state, Normal) = 
    ret (exps, stack', (state, Normal))
        where stack' = StackInteger (a / b):stack
istep (TRUE _:exps) stack state@(_, Normal) = 
    ret (exps, (StackBool $ absBool $ Just True):stack, state)
istep (FALSE _:exps) stack state@(_, Normal) = 
    ret (exps, (StackBool $ absBool $ Just False):stack, state)
istep (EQUAL _:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) =
    ret (exps, stack', state)
        where stack' = StackBool (a == b):stack
istep (LE _:exps) (StackInteger a:StackInteger b:stack) state@(_, Normal) =
    ret (exps, stack', state)
        where stack' = StackBool (a <= b):stack
istep (AND _:exps) (StackBool a:StackBool b:stack) state@(_, Normal) =
    ret (exps, stack', state)
        where stack' = StackBool (a && b):stack
istep (NEG _:exps) (StackBool a:stack) state@(_, Normal) =
    ret (exps, stack', state)
        where stack' = StackBool (neg a):stack
istep (BRANCH s1 s2 _:exps) (StackBool b:stack) (state, Normal) =
    case isBottom b of
        Yes -> ret (exps, stack, (state, Exception))
        No -> Right set'
        Maybe -> Right set
    where
        branches = cond b s1 s2
        set' = Set.fromList $ 
                    map (\c -> (c ++ exps, stack, (state, Normal))) branches
        set = (Set.insert (s2 ++ exps, stack, (state, Normal))) $
              (Set.insert (s1 ++ exps, stack, (state, Normal))) $
              (Set.insert (exps, stack, (state, Exception))) $
               Set.empty
istep (LOOP b s cp:exps) stack state@(_, Normal) =
    ret (exps', stack, state)
        where exps' = b ++ [(BRANCH (s ++ [LOOP b s cp]) [NOOP cp] cp)] ++ exps
istep (TRY s1 s2 cp:exps) stack state@(_, Normal) =
    ret (s1 ++ (CATCH s2 cp):exps, stack, state)
istep (CATCH _ _:exps) stack state@(_, Normal) =
    ret (exps, stack, state)
istep (CATCH s _:exps) stack (state, Exception) =
    ret ((s ++ exps), stack, (state, Normal))
istep (_:exps) stack state@(_, Exception) =
    ret (exps, stack, state)
istep exps stack state = Left $ "Encountered bad configuration: \n" ++
                                "\tCode\n\t:" ++ (show exps) ++
                                "\tStack\n\t" ++ (show stack) ++
                                "\tState\n\t" ++ (show state)

update :: (AMNum a) => String -> a -> State a -> State a
update x n s = 
    case Map.member x s of
        True -> Map.update (\_ -> Just n) x s
        False -> Map.insert x n s

toState :: (AMNum a) => [(String, a)] -> (State a, StateMode)
toState list = (Map.fromList list, Normal)
