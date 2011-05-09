module Wham.SignBoolExcType where

data SignBoolExc = NoneT
                 | AnyT
                 | ErrorT
                 | NonErrorT
                 | TT
                 | FF
                 deriving (Show)
