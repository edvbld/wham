module Main(main) where

import System.Environment (getArgs)
import Data.List (sort)
import System.Console.GetOpt
import Wham.Parser
import Wham.Translator
import Wham.Evaluator
import Wham.Debugger

main :: IO()
main = do args <- getArgs
          case getOpt RequireOrder options args of
            (flags, [fname], []) -> do content <- readFile fname
                                       eval fname (sort flags) content
                                       `catch`
                                       (\_ -> 
                                          error $ "Problem with file " ++ fname)
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
    Right stmt -> do let code = translate stmt
                     if shouldDebug
                        then debug code vars
                        else case evaluate code vars of
                                Left err -> print $ "Error: " ++ err
                                Right (state, mode) -> 
                                  putStrLn $ (show mode) ++ ": " ++ (show state)
    Left err -> print err

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
