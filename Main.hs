module Main(main) where

import System.Environment (getArgs)
import Data.List (sort)
import System.Console.GetOpt
import Parser
import Translator
import Interpreter

main :: IO()
main = do args <- getArgs
          case getOpt RequireOrder options args of
            (flags, [fname], []) -> do content <- readFile fname
                                       eval fname (sort flags) content
            (_, _, msgs) -> error $ concat msgs ++ usageInfo header options
          where

eval :: String -> [Flag] -> String -> IO()
eval fname [] content = run fname content [] False
eval fname [State s] content = run fname content s False
eval fname [Debug] content = run fname content [] True
eval fname [Debug, State s] content = run fname content s True
eval _ _ _ = error $ usageInfo header options

run :: String -> String -> [(String, Integer)] -> Bool -> IO()
run fname content vars shouldDebug = case parse parser fname content of 
    Right stmt -> do let exps = translate stmt
                     if shouldDebug
                        then debug exps vars
                        else case evaluate (translate stmt, [], toState vars) of
                                Left err -> print $ "Error: " ++ err
                                Right c -> print c
    Left err -> print err

debug :: [AMExpression] -> [(String, Integer)] -> IO()
debug exps vars = impl (exps, [], (toState vars))
    where
        impl ([], _, _) = return ()
        impl c = do case step c of 
                        Left err -> print $ "Error: " ++ err
                        Right c' -> do print c'
                                       _ <- getLine
                                       impl c'

data Flag = Debug | State [(String, Integer)] deriving (Eq, Show, Ord)

readState :: String -> Flag
readState s = State vars
    where
        list = (reads s)::[([(String, Integer)], String)]
        vars = if length list /= 1 then [] else fst $ head list

options :: [OptDescr Flag]
options = [Option ['d'] ["debug"] (NoArg Debug) 
           "show debug info",
           Option ['s'] ["state"] (ReqArg readState "state") 
           "evaluate from the given state"]

header :: String
header = "[OPTIONS] file-name"
