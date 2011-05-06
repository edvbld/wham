{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module AMDefinitions where

data AMExpression = PUSH Integer
                  | FETCH String
                  | STORE String
                  | BRANCH [AMExpression] [AMExpression]
                  | LOOP [AMExpression] [AMExpression]
                  | TRY [AMExpression] [AMExpression]
                  | CATCH [AMExpression]
                  | NOOP
                  | TRUE
                  | FALSE
                  | ADD
                  | SUB
                  | MULT
                  | DIV
                  | NEG
                  | EQUAL
                  | LE
                  | AND
                    deriving (Show)

class (Show a) => AMNum a b | a -> b where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    (/) :: a -> a -> a
    absInteger :: Integer -> a
    (<=) :: a -> a -> b
    (==) :: a -> a -> b

class (Show b) => AMBoolean b where
    neg :: b -> b
    (&&) :: b -> b -> b
    absBool :: Bool -> b
    toBool :: b -> Bool

instance AMBoolean Bool where
    neg b = not b
    a && b = (Prelude.&&) a b
    absBool b = b
    toBool b = b

instance AMNum Integer Bool where
    a + b = (Prelude.+) a b
    a * b = (Prelude.*) a b
    a - b = (Prelude.-) a b
    a / b = Prelude.div a b
    absInteger a = a
    a <= b = (Prelude.<=) a b
    a == b = (Prelude.==) a b
