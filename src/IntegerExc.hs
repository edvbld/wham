{-# LANGUAGE MultiParamTypeClasses #-}
module IntegerExc where

import AMDefinitions
import BoolExc

data IntegerExc = Integer Integer
                | IntegerBottom

instance Show IntegerExc where
    show (Integer i) = show i
    show IntegerBottom = "Bottom"

instance HasBottom IntegerExc where
    isBottom IntegerBottom = True
    isBottom _ = False

instance AMNum IntegerExc BoolExc where
    (Integer a) + (Integer b) = Integer $ (Prelude.+) a b
    _ + _ = IntegerBottom
    (Integer a) * (Integer b) = Integer $ (Prelude.*) a b
    _ * _ = IntegerBottom
    (Integer a) - (Integer b) = Integer $ (Prelude.-) a b
    _ - _ = IntegerBottom
    (Integer _) / (Integer 0) = IntegerBottom
    (Integer a) / (Integer b) = Integer $ Prelude.div a b
    _ / _ = IntegerBottom
    absInteger a = Integer a
    (Integer a) <= (Integer b) = Bool $ (Prelude.<=) a b
    _ <= _ = BoolBottom
    (Integer a) == (Integer b) = Bool $ (Prelude.==) a b
    _ == _ = BoolBottom
