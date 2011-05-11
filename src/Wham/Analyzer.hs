module Wham.Analyzer(analyze) where

import Wham.AnalyzeRunner
import Wham.AMDefinitions hiding ((==))
import Wham.SignExc
import Wham.SignBoolExc
import Wham.InterpreterTypes
import qualified Data.Map as Map
import qualified Data.Set as Set

analyze :: [AMExpression] -> [(String, Integer)] -> 
           Either String (Map.Map Integer (State SignExc))
analyze code st = 
    case mcp of 
        Right cpmap -> Right $ Map.map lub cpmap
        Left err -> Left err
    where
        state' = map (\(s,i) -> (s, absInteger $ Just i)) st
        mcp = run (Map.singleton 0 [(code, [], toState state')]) 
                  (Map.singleton 0 (Set.singleton ([], [], (Map.empty, Normal))))

lub :: ConfigurationSet -> State SignExc
lub set = s
    where 
        n = map state $ filter normal $ Set.toList set
        s = foldl ilub (head n) n

ilub :: State SignExc -> State SignExc -> State SignExc
ilub s1 s2 = Map.unionWith elub s1 s2

elub :: SignExc -> SignExc -> SignExc
a           `elub` NoneS       = a
NoneS       `elub` a           = a
AnyS        `elub` _           = AnyS
_           `elub` AnyS        = AnyS
ErrorS      `elub` ErrorS      = ErrorS
_           `elub` ErrorS      = AnyS
ErrorS      `elub` _           = AnyS
NonErrorS   `elub` _           = NonErrorS
_           `elub` NonErrorS   = NonErrorS
NonNegative `elub` Negative    = NonErrorS
NonNegative `elub` Zero        = NonNegative
NonNegative `elub` Positive    = NonNegative
NonNegative `elub` NonPositive = NonErrorS
NonNegative `elub` NonZero     = NonErrorS
NonNegative `elub` NonNegative = NonNegative
NonPositive `elub` Negative    = NonPositive
NonPositive `elub` Zero        = NonPositive
NonPositive `elub` Positive    = NonErrorS
NonPositive `elub` NonZero     = NonErrorS
NonPositive `elub` NonPositive = NonPositive
NonZero     `elub` Negative    = NonZero
NonZero     `elub` Zero        = NonErrorS
NonZero     `elub` Positive    = NonZero
NonZero     `elub` NonZero     = NonZero
Positive    `elub` Zero        = NonNegative
Positive    `elub` Negative    = NonZero
Positive    `elub` Positive    = Positive
Negative    `elub` Zero        = NonPositive
Negative    `elub` Negative    = Negative
Zero        `elub` Zero        = Zero
a           `elub` b           = b `elub` a

{-
            NONE_A    NEG       ZERO     POS       A_ERR  NON_POS  NON_ZERO  NON_NEG  Z       ANY_A 
  NONE_A  { NONE_A,   NEG,      ZERO,    POS,      ERR_A, NON_POS, NON_ZERO, NON_NEG, Z,      ANY_A },
     NEG  { NEG,      NEG,      NON_POS, NON_ZERO, ANY_A, NON_POS, NON_ZERO, Z,       Z,      ANY_A },
    ZERO  { ZERO,     NON_POS,  ZERO,    NON_NEG,  ANY_A, NON_POS, Z,        NON_NEG, Z,      ANY_A },
     POS  { POS,      NON_ZERO, NON_NEG, POS,      ANY_A, Z,       NON_ZERO, NON_NEG, Z,      ANY_A },
   A_ERR  { ERR_A,    ANY_A,    ANY_A,   ANY_A,    ERR_A, ANY_A,   ANY_A,    ANY_A,   ANY_A,  ANY_A },
 NON_POS  { NON_POS,  NON_POS,  NON_POS, Z,        ANY_A, NON_POS, Z,        Z,       Z,      ANY_A },
NON_ZERO  { NON_ZERO, NON_ZERO, Z,       NON_ZERO, ANY_A, Z,       NON_ZERO, Z,       Z,      ANY_A },
 NON_NEG  { NON_NEG,  Z,        NON_NEG, NON_NEG,  ANY_A, Z,       Z,        NON_NEG, Z,      ANY_A },
       Z  { Z,        Z,        Z,       Z,        ANY_A, Z,       Z,        Z,       Z,      ANY_A },
   ANY_A  { ANY_A,    ANY_A,    ANY_A,   ANY_A,    ANY_A, ANY_A,   ANY_A,    ANY_A,   ANY_A,  ANY_A }  
-}

normal :: Configuration SignExc SignBoolExc -> Bool
normal (_,_,(_,m)) = m == Normal

state :: Configuration SignExc SignBoolExc -> State SignExc
state (_,_,(s,_)) = s

