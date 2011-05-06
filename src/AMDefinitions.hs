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

data IntegerExc = Integer Integer
                | IntegerBottom
                deriving (Show)

data BoolExc = Bool Bool
             | BoolBottom
             deriving (Show)

instance HasBottom BoolExc where
    isBottom BoolBottom = True
    isBottom _ = False

instance AMBoolean BoolExc where
    (Bool a) && (Bool b) = Bool $ (Prelude.&&) a b
    _ && _ = BoolBottom
    neg (Bool b) = Bool $ not b
    neg _ = BoolBottom
    absBool b = (Bool b)
    cond (Bool b) s1 s2 = if b then s1 else s2
    cond BoolBottom _ _ = []

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
