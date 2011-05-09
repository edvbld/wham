module Wham.SignBoolExc (SignBoolExc(..)) where

import Prelude hiding (not, and)
import Wham.AMDefinitions
import Wham.SignBoolExcType

and :: SignBoolExc -> SignBoolExc -> SignBoolExc
NoneT `and` _ = NoneT
AnyT `and` NoneT = NoneT 
AnyT `and` ErrorT = ErrorT
AnyT `and` _ = AnyT
ErrorT `and` NoneT = NoneT
ErrorT `and` _ = ErrorT
TT `and` TT = TT
TT `and` FF = FF
TT `and` NonErrorT = NonErrorT
FF `and` FF = FF
FF `and` NonErrorT = NonErrorT
NonErrorT `and` NonErrorT = NonErrorT
a `and` b = b `and` a

not :: SignBoolExc -> SignBoolExc
not TT = FF
not FF = TT
not x = x

fromBool :: Maybe Bool -> SignBoolExc
fromBool (Just True) = TT
fromBool (Just False) = FF
fromBool Nothing = ErrorT

instance HasBottom SignBoolExc where
    isBottom AnyT = True
    isBottom ErrorT = True
    isBottom _ = False

instance AMBoolean SignBoolExc where
    (&&) = and
    neg = not
    absBool = fromBool
    absSignBoolExc = id
    -- TODO: Define a better cond function
    cond TT s1 _ = s1
    cond FF _ s2 = s2
    cond _  _ s2 = s2
