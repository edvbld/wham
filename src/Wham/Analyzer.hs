module Wham.Analyzer(analyze) where

import Wham.SignExc
import Wham.SignBoolExc
import Wham.Interpreter
import Wham.AMDefinitions

analyze :: [AMExpression] -> [(String, Integer)] -> String
analyze _ _ = "Analyzing!"

