module Main where

import Parser
import Translator
import Interpreter

run s = 
    case parse s of
        Right stmt -> evaluate (translate stmt) []
