module Wham.Debugger (debug) where

import Wham.Interpreter
import Wham.AMDefinitions
import Wham.IntegerExc
import Wham.BoolExc

debug :: [AMExpression] -> [(String, Integer)] -> IO()
debug exps state = dstep (exps, [], toState state')
    where state' = map (\(s, i) -> (s, absInteger $ Just i)) state

dstep :: Configuration IntegerExc BoolExc -> IO()
dstep ([], [], _) = return ()
dstep c = do case step c of 
              Left err -> print $ "Error: " ++ err
              Right c' -> do print c'
                             _ <- getLine
                             dstep c'