module Wham.Debugger (debug) where

import qualified Data.Set as Set
import Wham.Interpreter
import Wham.AMDefinitions hiding ((==))
import Wham.IntegerExc
import Wham.BoolExc

debug :: [AMExpression] -> [(String, Integer)] -> IO()
debug exps state = dstep (exps, [], toState state')
    where state' = map (\(s, i) -> (s, absInteger $ Just i)) state

dstep :: Configuration IntegerExc BoolExc -> IO()
dstep ([], [], _) = return ()
dstep c = do case step c of 
              Left err -> print $ "Error: " ++ err
              Right c' -> if Set.size c' == 1 
                            then 
                                do let res = head $ Set.elems c'
                                   print $ res
                                   _ <- getLine
                                   dstep res
                            else
                                print $ "Error: Multiple configurations!"
