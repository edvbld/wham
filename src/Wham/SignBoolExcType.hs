module Wham.SignBoolExcType where

data SignBoolExc = NoneT
                 | FF
                 | TT
                 | NonErrorT
                 | ErrorT
                 | AnyT
                 deriving (Show, Eq, Ord)
