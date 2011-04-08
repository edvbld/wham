module Main(main) where

import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Data.List (sort)
import System.Console.GetOpt
import Parser
import Translator
import Interpreter

main :: IO()
main = do args <- getArgs
          case getOpt RequireOrder options args of
            (flags, [fname], []) -> do content <- readFile fname
                                       eval (sort flags) content
            (_, _, msgs) -> error $ concat msgs ++ usageInfo header options
          where

eval :: [Flag] -> String -> IO()
eval [] content = run content [] False
eval [State s] content = run content s False
eval [Debug] content = run content [] True
eval [Debug, State s] content = run content s True

run :: String -> [(String, Integer)] -> Bool -> IO()
run content state shouldDebug = case parse content of 
    Right stmt -> do let exps = translate stmt
                     if shouldDebug
                        then debug exps state
                        else print (evaluate (translate stmt) state)
    Left err -> print err

debug :: [AMExpression] -> [(String, Integer)] -> IO()
debug exps list = impl (exps, [], (state list))
    where
        impl ([], _, state) = return ()
        impl c = do let c' = step c
                    print c'
                    getLine
                    impl c'

data Flag = Debug | State [(String, Integer)] deriving (Eq, Show, Ord)

options :: [OptDescr Flag]
options = [Option ['d'] ["debug"] (NoArg Debug) 
           "show debug info",
           Option ['s'] ["state"] (ReqArg makeState "state") 
           "evaluate from the given state"]

makeState :: String -> Flag
makeState s = State ((read s)::[(String, Integer)])

header :: String
header = "[OPTIONS] file-name"
