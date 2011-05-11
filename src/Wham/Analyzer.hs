module Wham.Analyzer(analyze) where

import Wham.AnalyzeRunner
import Wham.AMDefinitions hiding ((==))
import Wham.SignExc
import Wham.SignBoolExcType
import Wham.InterpreterTypes
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)
import Debug.Trace

type AbstractStack = Stack SignExc SignBoolExc
type AbstractStackElement = StackElement SignExc SignBoolExc
type AbstractState = State SignExc
type AnalysisMap = 
    Map.Map Integer (Maybe AbstractStackElement, AbstractState, AbstractMode)
analyze :: [AMExpression] -> [(String, Integer)] -> 
           Either String AnalysisMap
analyze code st = 
    case mcp of 
        Right cpmap -> Right $ res cpmap
        Left err -> Left err
    where
        state' = map (\(s,i) -> (s, absInteger $ Just i)) st
        mcp = run (Map.singleton 0 [(code, [], toState state')]) 
                  (Map.singleton 0 (Set.singleton ([], [], (Map.empty, Normal))))
        stateMap cpm = Map.map confToState cpm
        stackMap cpm = Map.map toStackElement cpm
        modeMap cpm = Map.map toMode cpm
        res cpm = union (stackMap cpm) (stateMap cpm) (modeMap cpm) 

union :: Map.Map Integer (Maybe AbstractStackElement) -> 
         Map.Map Integer AbstractState ->
         Map.Map Integer AbstractMode ->
         Map.Map Integer (Maybe AbstractStackElement, AbstractState, AbstractMode)
union a b c = d
    where
        tmp = Map.intersectionWith (\x y -> (x,y)) b c
        d = Map.intersectionWith (\x (y, z) -> (x,y,z)) a tmp

toMode :: ConfigurationSet -> AbstractMode
toMode set = s
    where
        modes = nub $ map mode $ Set.toList set
        s = case modes of
                [a] -> StateMode a
                [_,_] -> Both
                _ -> Both {- TODO: This shouldn't happen -}

toStackElement :: ConfigurationSet -> Maybe AbstractStackElement
toStackElement set = s
    where
        stacks = map (\(h:_) -> h) $ 
                 filter (\l -> length l /= 0) $ 
                 map stack $ 
                 filter normal $ 
                 Set.toList set
        s = case stacks of 
              [] -> Nothing
              list  -> Just $ foldl slub (head list) list

slub :: AbstractStackElement -> AbstractStackElement -> AbstractStackElement
slub (StackInteger a) (StackInteger b) = StackInteger $ elub a b
slub (StackBool a) (StackBool b) = StackBool $ blub a b
slub a _ = a

confToState :: ConfigurationSet -> State SignExc
confToState set = s
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

blub :: SignBoolExc -> SignBoolExc -> SignBoolExc
AnyT      `blub` _         = AnyT
_         `blub` AnyT      = AnyT
NoneT     `blub` a         = a
a         `blub` NoneT     = a
ErrorT    `blub` ErrorT    = ErrorT
_         `blub` ErrorT    = AnyT
ErrorT    `blub` _         = AnyT
NonErrorT `blub` _         = NonErrorT
_         `blub` NonErrorT = NonErrorT
TT        `blub` TT        = TT
TT        `blub` FF        = NonErrorT
FF        `blub` FF        = FF
FF        `blub` TT        = NonErrorT

{-
          NONE_B  TT      FF      ERR_B   T       ANY_B
NONE_B  { NONE_B, TT,     FF,     ERR_B,  T,      ANY_B  },
TT      { TT,     TT,     T,      ANY_B,  T,      ANY_B  },
FF      { FF,     T,      FF,     ANY_B,  T,      ANY_B  },
ERR_B   { ERR_B,  ANY_B,  ANY_B,  ERR_B,  ANY_B,  ANY_B  },
T       { T,      T,      T,      ANY_B,  T,      ANY_B  },
ANY_B   { ANY_B,  ANY_B,  ANY_B,  ANY_B,  ANY_B,  ANY_B  }
-}

normal :: Configuration SignExc SignBoolExc -> Bool
normal (_,_,(_,m)) = m == Normal

state :: Configuration SignExc SignBoolExc -> State SignExc
state (_,_,(s,_)) = s

stack :: Configuration SignExc SignBoolExc -> AbstractStack
stack (_, s, _) = s

mode :: Configuration SignExc SignBoolExc -> StateMode
mode (_, _, (_, m)) = m
