module Wham.Translator(translate) where

import Wham.AST
import Wham.ControlPoint
import Wham.AMDefinitions hiding ((+))

addControlPoints :: Statement -> ControlPoint -> StatementCP
addControlPoints (Skip) cp = SkipCP cp
addControlPoints (Assign x a) cp = AssignCP x a cp
addControlPoints (Compound s1 s2) cp = CompoundCP (addControlPoints s1 cp) 
                                                  (addControlPoints s2 $ cp + 1)
addControlPoints (If b s1 s2) cp = IfCP b (addControlPoints s1 $ cp + 1) 
                                          (addControlPoints s2 $ cp + 2) cp
addControlPoints (While b s) cp = WhileCP b (addControlPoints s $ cp + 1) cp
addControlPoints (TryCatch s1 s2) cp = TryCatchCP 
                                        (addControlPoints s1 $ cp + 1) 
                                        (addControlPoints s2 $ cp + 2) cp

translate :: Statement -> [AMExpression]
translate s = translateStatement $ addControlPoints s 0 

translateArithmetic :: ArithmeticExp -> ControlPoint -> [AMExpression]
translateArithmetic (Number n) cp = [(PUSH n cp)]
translateArithmetic (Variable x) cp = [(FETCH x cp)]
translateArithmetic (Add a1 a2) cp = (translateArithmetic a2 cp) ++  
                                     (translateArithmetic a1 cp) ++
                                     [ADD cp]
translateArithmetic (Sub a1 a2) cp = (translateArithmetic a2 cp) ++  
                                     (translateArithmetic a1 cp) ++
                                     [SUB cp]
translateArithmetic (Mul a1 a2) cp = (translateArithmetic a2 cp) ++  
                                     (translateArithmetic a1 cp) ++
                                     [MULT cp]
translateArithmetic (Div a1 a2) cp = (translateArithmetic a2 cp) ++  
                                     (translateArithmetic a1 cp) ++
                                     [DIV cp]

translateBoolean :: BooleanExp -> ControlPoint -> [AMExpression]
translateBoolean (Boolean True) cp = [TRUE cp]
translateBoolean (Boolean False) cp = [FALSE cp]
translateBoolean (Not b) cp = (translateBoolean b cp) ++ [NEG cp]
translateBoolean (Equal a1 a2) cp = (translateArithmetic a2 cp) ++
                                    (translateArithmetic a1 cp) ++
                                    [EQUAL cp]
translateBoolean (LessOrEqual a1 a2) cp = (translateArithmetic a2 cp) ++
                                          (translateArithmetic a1 cp) ++
                                          [LE cp]
translateBoolean (And b1 b2) cp = (translateBoolean b2 cp) ++
                                  (translateBoolean b1 cp) ++
                                  [AND cp]

translateStatement :: StatementCP -> [AMExpression]
translateStatement (SkipCP cp) = [NOOP cp]
translateStatement (AssignCP x a cp) = (translateArithmetic a cp) ++ 
                                       [(STORE x cp)]
translateStatement (CompoundCP s1 s2) = (translateStatement s1) ++
                                        (translateStatement s2)
translateStatement (IfCP b s1 s2 cp) = (translateBoolean b cp) ++
                                       [BRANCH (translateStatement s1) 
                                               (translateStatement s2) cp]
translateStatement (WhileCP b s cp) = [LOOP (translateBoolean b cp) 
                                            (translateStatement s) cp]
translateStatement (TryCatchCP s1 s2 cp) = [TRY (translateStatement s1)
                                                (translateStatement s2) cp]
