module Main(main) where

import System.Environment (getArgs)
import Data.List (sort)
import System.Console.GetOpt
import Wham.Parser
import Wham.Translator
import Wham.Evaluator
import Wham.Debugger
import Wham.AnalyzeRunner
import qualified Data.Map as Map
import qualified Data.Set as Set

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

data Action = Run | Step | Analyze

eval :: String -> [Flag] -> String -> IO()
eval fname [] content = run fname content [] Run
eval fname [State s] content = run fname content s Run
eval fname [Debug] content = run fname content [] Step
eval fname [Debug, State s] content = run fname content s Step
eval fname [Analysis] content = run fname content [] Analyze
eval fname [Analysis, State s] content = run fname content s Analyze
eval _ _ _ = error $ usageInfo header options

run :: String -> String -> [(String, Integer)] -> Action -> IO()
run fname content vars action = case parse parser fname content of 
    Right stmt -> do let code = translate stmt
                     case action of
                        Step -> debug code vars
                        Run  -> case evaluate code vars of
                                  Left err -> print $ "Error: " ++ err
                                  Right (state, mode) -> 
                                     putStrLn $ 
                                        (show mode) ++ ": " ++ (show state)
                        Analyze -> do print code
                                      case analyze code vars of
                                        Right res -> do mapM_ (\(i, s) -> mapM_ (\(_, _, s') -> print (i,s')) (Set.toList s)) (Map.toList res)
                                        Left err -> print err
    Left err -> print err

data Flag = Analysis | Debug | State [(String, Integer)] 
            deriving (Eq, Show, Ord)

readState :: String -> Flag
readState s = State vars
    where
        list = (reads s)::[([(String, Integer)], String)]
        vars = if length list /= 1 then [] else fst $ head list

options :: [OptDescr Flag]
options = [Option ['d'] ["debug"] (NoArg Debug) 
           "show debug info",
           Option ['s'] ["state"] (ReqArg readState "state") 
           "evaluate from the given state",
           Option ['a'] ["analyze"] (NoArg Analysis)
           "run the static analyzer"]

header :: String
header = "[OPTIONS] file-name"
