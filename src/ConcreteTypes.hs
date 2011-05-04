module ConcreteTypes where 

{-import qualified Prelude as P hiding ((<=), (==))-}
import AMDefinitions

instance AMBoolean Bool where
    neg b = neg b
    (&&) a b = (Prelude.&&) a b
    fromBool b = b
    toBool b = b

instance AMNum Integer where
    (+) a b = (Prelude.+) a b
    (*) a b = (Prelude.*) a b
    (-) a b = (Prelude.-) a b
    (/) a b = Prelude.div a b
    fromInteger a = a
    (<=) a b = (AMDefinitions.<=) a b
    (==) a b = (AMDefinitions.==) a b
