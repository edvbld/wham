module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

-- lexer

lexer :: P.TokenParser ()
lexer = P.makeTokenParser whileDef

whileDef = (emptyDef
              {P.reservedOpNames = ["*", "+", "-", "!", "&", "=", "<="],
               P.reservedNames = ["true", "false"],
               P.identStart = letter
              })

identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
integer = P.integer lexer

-- parser 

data ArithmeticExp = Number Integer
                   | Variable String
                   | Add ArithmeticExp ArithmeticExp
                   | Mul ArithmeticExp ArithmeticExp
                   | Sub ArithmeticExp ArithmeticExp
                   deriving (Show)

data BooleanExp = Boolean Bool
                | Not BooleanExp
                | And BooleanExp BooleanExp
                | Equal ArithmeticExp ArithmeticExp
                | LessOrEqual ArithmeticExp ArithmeticExp
                deriving (Show)

aexp = buildExpressionParser arithmeticOperators arithmetic
bexp = buildExpressionParser booleanOperators boolean

boolean :: Parser BooleanExp
boolean =   true 
        <|> false
        <|> arithmeticComparison

booleanOperators :: OperatorTable Char () BooleanExp
booleanOperators = [[prefix "!" Parser.not],
                    [binary "&" Parser.and AssocLeft]]

arithmeticComparison :: Parser BooleanExp
arithmeticComparison = do { l <- aexp;
                            do { reservedOp "=";
                                 equals l}
                          <|>
                            do { reservedOp "<=";
                                 lessOrEquals l}
                           }

equals :: ArithmeticExp -> Parser BooleanExp
equals l  = do r <- aexp
               return (Equal l r)

lessOrEquals :: ArithmeticExp -> Parser BooleanExp
lessOrEquals l = do r <- aexp
                    return (LessOrEqual l r)

not :: BooleanExp -> BooleanExp
not b = Not b

and :: BooleanExp -> BooleanExp -> BooleanExp
and l r = And l r

true :: Parser BooleanExp
true = do reserved "true"
          return (Boolean True)

false :: Parser BooleanExp
false = do reserved "false"
           return (Boolean False)

arithmetic :: Parser ArithmeticExp
arithmetic = number <|> variable

number :: Parser ArithmeticExp
number = do n <- integer
            return (Number n)

variable :: Parser ArithmeticExp
variable = do var <- identifier
              return (Variable var)

arithmeticOperators :: OperatorTable Char () ArithmeticExp
arithmeticOperators = [[binary "*" mul AssocLeft],
                       [binary "+" add AssocLeft, binary "-" sub AssocLeft]]

binary symbol operation assoc = Infix (do {reservedOp symbol; 
                                           return operation}) 
                                assoc

prefix symbol operation = Prefix (do { reservedOp symbol; 
                                       return operation })

mul :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp 
mul l r = Mul l r

add :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp 
add l r = Add l r

sub :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp 
sub l r = Sub l r
