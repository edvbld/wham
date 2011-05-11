module Wham.Translator(translate, annotate) where

import Wham.AST
import Wham.ControlPoint
import Wham.AMDefinitions hiding ((+))

addCP :: Statement -> ControlPoint -> (ControlPoint, StatementCP)
addCP (Skip) cp = (cp + 1, SkipCP cp)
addCP (Assign x a) cp = (cp + 1, AssignCP x a cp)
addCP (Compound s1 s2) cp = (cp2, CompoundCP stm1 stm2)
    where
        (cp1, stm1) = addCP s1 cp
        (cp2, stm2) = addCP s2 cp1
addCP (If b s1 s2) cp = (cp2, IfCP b stm1 stm2 cp)
    where
        (cp1, stm1) = addCP s1 $ cp + 1
        (cp2, stm2) = addCP s2 cp1
addCP (While b s) cp = (cp1 + 1, WhileCP b stm cp)
    where
        (cp1, stm) = addCP s $ cp + 1
addCP (TryCatch s1 s2) cp = (cp2, TryCatchCP stm1 stm2 cp)
    where
        (cp1, stm1) = addCP s1 $ cp + 1
        (cp2, stm2) = addCP s2 cp1

annotate :: Statement -> StatementCP
annotate s = stm
    where (_, stm) = addCP s 0

translate :: StatementCP -> [AMExpression]
translate s = translateStatement s

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
