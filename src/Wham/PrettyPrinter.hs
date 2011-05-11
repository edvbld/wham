module Wham.PrettyPrinter (pprint) where

import Wham.ControlPoint
import Wham.InterpreterTypes
import Wham.SignExc
import Wham.AST
import qualified Data.Map as Map

pprintA :: ArithmeticExp -> String
pprintA (Number i) = show i
pprintA (Variable x) = x
pprintA (Add a1 a2) = (pprintA a1) ++ " + " ++ (pprintA a2)
pprintA (Mul a1 a2) = (pprintA a1) ++ " * " ++ (pprintA a2)
pprintA (Sub a1 a2) = (pprintA a1) ++ " - " ++ (pprintA a2)
pprintA (Div a1 a2) = (pprintA a1) ++ " / " ++ (pprintA a2)

pprintB :: BooleanExp -> String
pprintB (Boolean b) = show b
pprintB (Not b1) = "!" ++ (pprintB b1)
pprintB (And b1 b2) = (pprintB b1) ++ " && " ++ (pprintB b2)
pprintB (Equal a1 a2) = (pprintA a1) ++ " == " ++ (pprintA a2)
pprintB (LessOrEqual a1 a2) = (pprintA a1) ++ " <= " ++ (pprintA a2)

pprint :: StatementCP -> (Map.Map Integer (State SignExc)) -> String
pprint (SkipCP cp) cpm = "skip"
pprint (AssignCP x a cp) cpm = x ++ " := " ++ (pprintA a)
pprint (CompoundCP s1 s2) cpm = str
    where
        str = (pprint s1 cpm) ++ ";\n" ++ (pprint s2 cpm)
pprint (IfCP b s1 s2 cp) cpm = str
    where
        str = "if " ++ (pprintB b) ++ "\n" ++ (indent 1 "then") ++ "\n" ++ 
              (indent 2 $ pprint s1 cpm) ++ "\n" ++ 
              (indent 1 "else") ++ "\n" ++ (indent 2 $ pprint s2 cpm)
pprint (WhileCP b body cp) cpm = str
    where
        str = "while " ++ (pprintB b) ++ " do\n" ++ (indent 1 $ pprint body cpm)
pprint (TryCatchCP s1 s2 cp) cpm = str
    where
        str = "try\n" ++ (indent 1 $ pprint s1 cpm) ++ 
              "\ncatch\n" ++ (indent 1 $ pprint s2 cpm)

indent :: Int -> (String -> String)
indent i = 
    reverse . tail . reverse . unlines . map (replicate (2*i) ' ' ++) . lines
