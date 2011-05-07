module Wham.BoolExc where

import Wham.AMDefinitions

data BoolExc = Bool Bool
             | BoolBottom

instance Show BoolExc where
    show (Bool b) = show b
    show BoolBottom = "Bottom"

instance HasBottom BoolExc where
    isBottom BoolBottom = True
    isBottom _ = False

instance AMBoolean BoolExc where
    (Bool a) && (Bool b) = Bool $ (Prelude.&&) a b
    _ && _ = BoolBottom
    neg (Bool b) = Bool $ not b
    neg _ = BoolBottom
    absBool (Just b) = (Bool b)
    absBool Nothing = BoolBottom
    cond (Bool b) s1 s2 = if b then s1 else s2
    cond BoolBottom _ _ = []

