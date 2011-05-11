module Wham.InterpreterTypes where 

import Wham.AMDefinitions
import qualified Data.Map as Map
import qualified Data.Set as Set

data StateMode = Normal 
               | Exception 
               deriving (Eq, Show, Ord)

data (Show a, Show b) => StackElement a b = StackInteger a
                                          | StackBool b
                                          deriving (Ord, Eq)

instance (Show a, Show b) => Show (StackElement a b) where
    show (StackInteger a) = show a
    show (StackBool b) = show b

type Stack a b = [StackElement a b]
type State a = Map.Map String a
type Result a b = Either String (Set.Set (Configuration a b))
type Configuration a b = ([AMExpression], Stack a b, (State a, StateMode))

toState :: (AMNum a) => [(String, a)] -> (State a, StateMode)
toState list = (Map.fromList list, Normal)
