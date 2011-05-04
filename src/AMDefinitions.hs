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

class (Show a) => AMNum a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    (/) :: a -> a -> a
    fromInteger :: Integer -> a
    (<=) :: (AMBoolean b) => a -> a -> b
    (==) :: (AMBoolean b) => a -> a -> b

class (Show b) => AMBoolean b where
    neg :: b -> b
    (&&) :: b -> b -> b
    fromBool :: Bool -> b
    toBool :: b -> Bool
