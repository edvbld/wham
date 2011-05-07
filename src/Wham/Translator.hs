module Wham.Translator(translate) where

import Wham.Parser
import Wham.AMDefinitions

translate :: Statement -> [AMExpression]
translate s = translateStatement s

translateArithmetic :: ArithmeticExp -> [AMExpression]
translateArithmetic (Number n) = [(PUSH n)]
translateArithmetic (Variable x) = [(FETCH x)]
translateArithmetic (Add a1 a2) = (translateArithmetic a2) ++  
                                  (translateArithmetic a1) ++
                                  [ADD]
translateArithmetic (Sub a1 a2) = (translateArithmetic a2) ++  
                                  (translateArithmetic a1) ++
                                  [SUB]
translateArithmetic (Mul a1 a2) = (translateArithmetic a2) ++  
                                  (translateArithmetic a1) ++
                                  [MULT]
translateArithmetic (Div a1 a2) = (translateArithmetic a2) ++  
                                  (translateArithmetic a1) ++
                                  [DIV]

translateBoolean :: BooleanExp -> [AMExpression]
translateBoolean (Boolean True) = [TRUE]
translateBoolean (Boolean False) = [FALSE]
translateBoolean (Not b) = (translateBoolean b) ++ [NEG]
translateBoolean (Equal a1 a2) = (translateArithmetic a2) ++
                                 (translateArithmetic a1) ++
                                 [EQUAL]
translateBoolean (LessOrEqual a1 a2) = (translateArithmetic a2) ++
                                       (translateArithmetic a1) ++
                                       [LE]
translateBoolean (And b1 b2) = (translateBoolean b2) ++
                               (translateBoolean b1) ++
                               [AND]

translateStatement :: Statement -> [AMExpression]
translateStatement (Skip) = [NOOP]
translateStatement (Assign x a) = (translateArithmetic a) ++ [(STORE x)]
translateStatement (Compound s1 s2) = (translateStatement s1) ++
                                      (translateStatement s2)
translateStatement (If b s1 s2) = (translateBoolean b) ++
                                  [BRANCH (translateStatement s1) 
                                          (translateStatement s2)]
translateStatement (While b s) = [LOOP (translateBoolean b) 
                                       (translateStatement s)]
translateStatement (TryCatch s1 s2) = [TRY (translateStatement s1)
                                           (translateStatement s2)]
