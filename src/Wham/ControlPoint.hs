module Wham.ControlPoint where

import Wham.AST

type ControlPoint = Integer
data StatementCP = SkipCP ControlPoint
                 | AssignCP String ArithmeticExp ControlPoint
                 | CompoundCP StatementCP StatementCP 
                 | IfCP BooleanExp StatementCP StatementCP ControlPoint
                 | WhileCP BooleanExp StatementCP ControlPoint
                 | TryCatchCP StatementCP StatementCP ControlPoint


