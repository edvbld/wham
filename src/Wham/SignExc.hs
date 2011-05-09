module Wham.SignExc (SignExc(..)) where

import Wham.AMDefinitions
import Prelude hiding (div)

data SignExc = Positive
             | Zero
             | Negative
             | NonNegative
             | NonPositive
             | NonZero
             | NonErrorS
             | ErrorS
             | AnyS
             | NoneS
             deriving (Show)

add :: SignExc -> SignExc -> SignExc
NoneS `add` _ = NoneS
ErrorS `add` NoneS = NoneS
ErrorS `add` _ = ErrorS
AnyS `add` ErrorS = ErrorS
AnyS `add` NoneS = NoneS
AnyS `add` _ = AnyS
NonErrorS `add` NonErrorS = NonErrorS
NonErrorS `add` ErrorS = ErrorS
NonErrorS `add` NoneS = NoneS
NonErrorS `add` AnyS = AnyS
NonErrorS `add` _ = NonErrorS
Positive `add` NoneS = NoneS
Positive `add` Negative = NonErrorS
Positive `add` Positive = Positive
Positive `add` Zero = Positive
Positive `add` NonPositive = AnyS
Positive `add` NonNegative = NonNegative
Positive `add` NonZero = NonZero
Positive `add` NonErrorS = NonErrorS
Positive `add` ErrorS = ErrorS
Positive `add` AnyS = AnyS
Negative `add` NoneS = NoneS
Negative `add` Zero = Negative
Negative `add` Negative = Negative
Negative `add` Positive = AnyS
Negative `add` NonNegative = AnyS
Negative `add` NonPositive = NonPositive
Negative `add` NonZero = NonZero
Negative `add` NonErrorS = NonErrorS
Negative `add` AnyS = AnyS
Negative `add` ErrorS = ErrorS
Zero `add` NoneS = NoneS
Zero `add` Zero = Zero
Zero `add` Negative = Negative
Zero `add` Positive = Positive
Zero `add` NonNegative = NonNegative
Zero `add` NonPositive = NonPositive
Zero `add` NonZero = AnyS
Zero `add` NonErrorS = NonErrorS
Zero `add` AnyS = AnyS
Zero `add` ErrorS = ErrorS
NonPositive `add` NonNegative = NonErrorS
NonPositive `add` NonZero = NonErrorS
NonPositive `add` NonPositive = NonPositive
NonNegative `add` NonNegative = NonNegative
NonNegative `add` NonZero = NonErrorS
NonNegative `add` NonPositive = NonErrorS
NonZero `add` NonNegative = NonErrorS
NonZero `add` NonZero = NonZero
NonZero `add` NonPositive = NonErrorS
a `add` b = b `add` a

mul :: SignExc -> SignExc -> SignExc
NoneS `mul` _ = NoneS
ErrorS `mul` NoneS = NoneS
ErrorS `mul` _ = ErrorS
AnyS `mul` ErrorS = ErrorS
AnyS `mul` NoneS = NoneS
AnyS `mul` _ = AnyS
NonErrorS `mul` Zero = Zero
NonErrorS `mul` ErrorS = ErrorS
NonErrorS `mul` NoneS = NoneS
NonErrorS `mul` AnyS = AnyS
NonErrorS `mul` _ = NonErrorS
Positive `mul` NoneS = NoneS
Positive `mul` Positive = Positive
Positive `mul` Negative = Negative
Positive `mul` Zero = Zero
Positive `mul` NonPositive = NonPositive
Positive `mul` NonNegative = NonPositive
Positive `mul` NonZero = NonErrorS
Positive `mul` NonErrorS = NonErrorS
Positive `mul` ErrorS = ErrorS
Positive `mul` AnyS = AnyS
Negative `mul` NoneS = NoneS
Negative `mul` Positive = Negative
Negative `mul` Negative = Positive
Negative `mul` Zero = Zero
Negative `mul` NonPositive = NonNegative
Negative `mul` NonNegative = NonPositive
Negative `mul` NonZero = NonZero
Negative `mul` NonErrorS = NonErrorS
Negative `mul` ErrorS = ErrorS
Negative `mul` AnyS = AnyS
Zero `mul` NoneS = NoneS
Zero `mul` ErrorS = ErrorS
Zero `mul` _ = Zero
NonPositive `mul` NonPositive = NonNegative
NonPositive `mul` NonNegative = NonErrorS
NonPositive `mul` NonZero = NonZero
NonNegative `mul` NonNegative = NonZero
NonNegative `mul` NonZero = NonZero
NonNegative `mul` NonPositive = NonErrorS
NonZero `mul` NonZero = NonZero
a `mul` b = b `mul` a

div :: SignExc -> SignExc -> SignExc
NoneS `div` _ = NoneS
_ `div` NoneS = NoneS
_ `div` Zero = ErrorS
ErrorS `div` _ = ErrorS
_ `div` ErrorS = ErrorS
_ `div` NonPositive = AnyS
_ `div` NonNegative = AnyS
_ `div` NonErrorS = AnyS
_ `div` AnyS = AnyS
Zero `div` NonZero = Zero 
Zero `div` Positive = Zero
Negative `div` Positive = NonPositive
Positive `div` Positive = NonNegative
NonPositive `div` Positive = NonPositive
NonZero `div` Positive = NonErrorS
NonNegative `div` Positive = NonNegative
NonErrorS `div` Positive = NonErrorS
Negative `div` Negative = NonNegative
Zero `div` Negative = Zero
Positive `div` Negative = NonPositive
NonPositive `div` Negative = NonNegative
NonZero `div` Negative = NonErrorS
NonNegative `div` Negative = NonPositive
NonErrorS `div` Negative = NonErrorS
NonPositive `div` NonZero = NonErrorS
NonZero `div` NonZero = NonErrorS
NonNegative `div` NonZero = NonErrorS
NonErrorS `div` NonZero = NonErrorS
a `div` b = b `div` a

sub :: SignExc -> SignExc -> SignExc
NoneS `sub` _ = NoneS
_ `sub` NoneS = NoneS
ErrorS `sub` _ = ErrorS
_ `sub` ErrorS = ErrorS
AnyS `sub` _ = AnyS
_ `sub` AnyS = AnyS
a `sub` Zero = a
Negative `sub` Negative = NonErrorS
Zero `sub` Negative = Positive
Positive `sub` Negative = Positive
NonPositive `sub` Negative = Positive
NonZero `sub` Negative = NonErrorS
NonNegative `sub` Negative = Positive
NonErrorS `sub` Negative = NonErrorS
Negative `sub` Positive = Negative
Zero `sub` Positive = Negative
Positive `sub` Positive = NonErrorS
NonPositive `sub` Positive = Negative
NonZero `sub` Positive = NonErrorS
NonNegative `sub` Positive = NonErrorS
NonErrorS `sub` Positive = NonErrorS
Negative `sub` NonPositive = NonErrorS
Zero `sub` NonPositive = NonNegative
Positive `sub` NonPositive = Positive
NonPositive `sub` NonPositive = NonErrorS
NonZero `sub` NonPositive = NonErrorS
NonNegative `sub` NonPositive = NonNegative
NonErrorS `sub` NonPositive = NonErrorS
Zero `sub` NonZero = NonZero
_ `sub` NonZero = NonErrorS 
Zero `sub` NonNegative = NonPositive
NonPositive `sub` NonNegative = NonPositive
_ `sub` NonNegative = NonErrorS
_ `sub` NonErrorS = NonErrorS

eq :: (AMBoolean b) => SignExc -> SignExc -> b
_ `eq` _ = absBool $ Just False

le :: (AMBoolean b) => SignExc -> SignExc -> b
_ `le` _ = absBool $ Just False

sign :: Maybe Integer -> SignExc
sign (Just n) 
    | n < 0 = Negative
    | (Prelude.==) n 0 = Zero
    | otherwise = Positive
sign Nothing = ErrorS

instance HasBottom SignExc where
    isBottom ErrorS = True
    isBottom _ = False

instance AMNum SignExc where
    a + b = a `add` b
    a * b = a `mul` b
    a - b = a `sub` b
    a / b = a `div` b
    a <= b = a `le` b
    a == b = a `eq` b
    absInteger = sign
