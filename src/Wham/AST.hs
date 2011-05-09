module Wham.AST where

data Statement = Skip
               | Assign String ArithmeticExp 
               | If BooleanExp Statement Statement 
               | While BooleanExp Statement 
               | Compound Statement Statement 
               | TryCatch Statement Statement
                 deriving (Show)

data ArithmeticExp = Number Integer 
                   | Variable String 
                   | Add ArithmeticExp ArithmeticExp 
                   | Mul ArithmeticExp ArithmeticExp 
                   | Sub ArithmeticExp ArithmeticExp 
                   | Div ArithmeticExp ArithmeticExp 
                     deriving (Show)

data BooleanExp = Boolean Bool 
                | Not BooleanExp
                | And BooleanExp BooleanExp
                | Equal ArithmeticExp ArithmeticExp
                | LessOrEqual ArithmeticExp ArithmeticExp
                  deriving (Show)
