{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Wham.AMDefinitions where

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

class HasBottom a where
    isBottom :: a -> Bool

class (HasBottom a, Show a) => AMNum a b | a -> b where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    (/) :: a -> a -> a
    (<=) :: a -> a -> b
    (==) :: a -> a -> b
    absInteger :: Integer -> a

class (HasBottom a, Show a) => AMBoolean a where
    (&&) :: a -> a -> a
    neg :: a -> a
    absBool :: Bool -> a
    cond :: a -> [AMExpression] -> [AMExpression] -> [AMExpression]
