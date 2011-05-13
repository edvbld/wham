module Wham.SignBoolExcType where

data SignBoolExc = NoneT
                 | FF
                 | TT
                 | NonErrorT
                 | ErrorT
                 | AnyT
                 deriving (Eq, Ord)

instance Show SignBoolExc where
    show NoneT = "none_t"
    show FF = "false"
    show TT = "true"
    show NonErrorT = "true or false"
    show ErrorT = "error_t"
    show AnyT = "any_t"
