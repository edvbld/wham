module Wham.SignExc where

import Wham.AMDefinitions
import Prelude hiding ((+))

data SignExc = Positive
             | Zero
             | Negative
             | NonNegative
             | NonPositive
             | NonZero
             | NonError
             | Error
             | Any
             | None
             deriving (Show)

instance HasBottom SignExc where
    isBottom Error = True
    isBottom _ = False

instance AMNum SignExc where
    
    None + _ = None
    Any + _ = Any

    NonError + NonError = NonError
    NonError + Error = Any
    NonError + None = None
    NonError + Any = Any
    NonError + _ = NonError

    Error + Error = Error

    Positive + None = None
    Positive + Negative = Any
    Positive + Positive = Positive
    Positive + Zero = Positive
    Positive + NonPositive = Any
    Positive + NonNegative = NonNegative
    Positive + NonZero = NonZero
    Positive + NonError = NonError
    Positive + Error = Any
    Positive + Any = Any
    
    Negative + None = None
    Negative + Zero = Negative
    Negative + Negative = Negative
    Negative + Positive = Any
    Negative + NonNegative = Any
    Negative + NonPositive = NonPositive
    Negative + NonZero = NonZero
    Negative + NonError = NonError
    Negative + Any = Any
    Negative + Error = Any

    Zero + None = None
    Zero + Zero = Zero
    Zero + Negative = Negative
    Zero + Positive = Positive
    Zero + NonNegative = NonNegative
    Zero + NonPositive = NonPositive
    Zero + NonZero = Any
    Zero + NonError = NonError
    Zero + Any = Any
    Zero + Error = Any
   
    a + b = b + a
