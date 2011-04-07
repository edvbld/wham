module Main(main) where

import System.Environment
import Parser
import Translator
import Interpreter

main :: IO()
main = do args <- getArgs
          if (length args) /= 1
            then putStrLn "usage: wham <name-of-file>"
            else do content <- readFile (head args)
                    case parse content of
                        Right stmt -> print (evaluate (translate stmt) [])
                        Left err -> print err
