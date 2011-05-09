module Wham.ControlPoint where

import Wham.Parser

type ControlPoint = Integer
data CPStatement = CPSkip
                 | CPAssign String ArithmeticExp ControlPoint
                 | CPCompound CPStatement CPStatement
                 | CPIf BooleanExp CPStatement CPStatement ControlPoint
                 | CPWhile BooleanExp CPStatement ControlPoint
                 | CPTryCatch CPStatement CPStatement ControlPoint

addCPs :: Statement -> ControlPoint -> CPStatement
addCPs (Skip) _ = CPSkip
addCPs (Assign x a) cp = CPAssign x a cp
addCPs (Compound s1 s2) cp = 
    CPCompound (addCPs s1 cp) (addCPs s2 $ cp + 1)
addCPs (If b s1 s2) cp = 
    CPIf b (addCPs s1 $ cp + 1) (addCPs s2 $ cp + 2) cp
addCPs (While b s) cp = CPWhile b (addCPs s $ cp + 1) cp
addCPs (TryCatch s1 s2) cp = 
    CPTryCatch (addCPs s1 $ cp + 1) (addCPs s2 $ cp + 2) cp

translateWithCPs :: CPStatement -> [CPAMExpression]
translateWithCPs (CPSkip) = [CPNOOP]
translateWithCPs (CPAssign x a cp) = 
    (translateArithmeticWithCP cp) ++ [(CPSTORE x cp)]
translateWithCPs (CPCompound c1 c2 cp) = 
    (translateWithCPs c1) ++ (translateWithCPs c2)
translateWithCPs (CPIf b s1 s2 cp) =
    (translateBooleanWithCP b cp) ++ [CPBRANCH (translateWithCPs c1) 
                                               (translateWithCPs c2) cp]
translateWithCPs (CPWhile b c cp) = [CPLOOP (translateBooleanWithCP b cp) 
                                            (translateWithCPs c) cp]
translateWithCPs (CPTryCatch c1 c2 cp) = [CPTRY (translateWithCPs c1)
                                                (translateWithCPs c2) cp]

translateBooleanWithCP :: BooleanExp -> ControlPoint -> [CPAMExpression]
translateBooleanWithCP (Boolean True) cp = [CPTRUE cp]
translateBooleanWithCP (Boolean False) cp = [CPFALSE cp]
translateBooleanWithCP (Not b) cp = (translateBooleanWithCP b cp) ++ [CPNOT cp]
translateBooleanWithCP (Equal a1 a2) cp = (translateArithmeticWithCP a2 cp) ++
                                          (translateArithmeticWithCP a1 cp) ++
                                          [CPEQUAL cp]
translateBooleanWithCP (LessOrEqual a1 a2) cp = 
    (translateArithmeticWithCP a2 cp) ++
    (translateArithmeticWithCP a1 cp) ++
    [CPLE cp]
translateArithmeticWithCP (And b1 b2) cp = (translateBooleanWithCP b2 cp) ++
                                           (translateBooleanWithCP b1 cp) ++
                                           [CPAND cp]

translateArithmeticWithCP :: ArithmeticExp -> ControlPoint -> [CPAMExpression]
translateArithmeticWithCP (Number n) cp = [CPPUSH n cp]
translateArithmeticWithCP (Variable x) cp = [CPFETCH x cp]
translateArithmeticWithCP (Add a1 a2) cp = (translateArithmeticWithCP a2 cp) ++
                                           (translateArithmeticWithCP a1 cp) ++
                                           [CPADD cp]
translateArithmeticWithCP (Sub a1 a2) cp = (translateArithmeticWithCP a2 cp) ++
                                           (translateArithmeticWithCP a1 cp) ++
                                           [CPSUB cp]
translateArithmeticWithCP (Mul a1 a2) cp = (translateArithmeticWithCP a2 cp) ++
                                           (translateArithmeticWithCP a1 cp) ++
                                           [CPMUL cp]
translateArithmeticWithCP (Div a1 a2) cp = (translateArithmeticWithCP a2 cp) ++
                                           (translateArithmeticWithCP a1 cp) ++
                                           [CPDIV cp]
