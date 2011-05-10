module Wham.BoolExc where

import Wham.AMDefinitions
import Wham.SignBoolExcType

data BoolExc = Bool Bool
             | BoolBottom
             deriving (Eq, Ord)

instance Show BoolExc where
    show (Bool b) = show b
    show BoolBottom = "Bottom"

instance HasBottom BoolExc where
    isBottom BoolBottom = Yes
    isBottom _ = No

instance AMBoolean BoolExc where
    (Bool a) && (Bool b) = Bool $ (Prelude.&&) a b
    _ && _ = BoolBottom
    neg (Bool b) = Bool $ not b
    neg _ = BoolBottom
    absBool (Just b) = (Bool b)
    absBool Nothing = BoolBottom
    absSignBoolExc TT = Bool True
    absSignBoolExc FF = Bool False
    absSignBoolExc ErrorT = BoolBottom
    absSignBoolExc _ = Bool False
    cond (Bool b) s1 s2 = if b then [s1] else [s2]
    cond BoolBottom _ _ = [[]] {- TODO: raise error here -}

