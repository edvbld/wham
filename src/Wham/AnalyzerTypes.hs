module Wham.AnalyzerTypes where

import Wham.SignExc
import Wham.SignBoolExc
import Wham.InterpreterTypes
import qualified Data.Map as Map

data AbstractMode = StateMode StateMode
                  | Both
                  deriving (Show)

type AbstractStack = Stack SignExc SignBoolExc
type AbstractStackElement = StackElement SignExc SignBoolExc
type AbstractState = State SignExc
type AnalysisMap = 
    Map.Map Integer (Maybe AbstractStackElement, AbstractState, AbstractMode)
