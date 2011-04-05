module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

-- lexer

lexer :: P.TokenParser ()
lexer = P.makeTokenParser whileDef

whileDef = (emptyDef
              {P.reservedOpNames = ["*", "+", "-", "!", "&", "=", "<=", ":=", 
                                    ";"],
               P.reservedNames = ["true", "false", "skip", "if", "then", 
                                  "else", "while", "do"],
               P.identStart = letter,
               P.commentLine = "#"
              })

identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
integer = P.integer lexer
parens = P.parens lexer

-- parser 

data Statement = Skip
               | Assign String ArithmeticExp
               | If BooleanExp Statement Statement 
               | While BooleanExp Statement
               | Compound Statement Statement
                 deriving (Show)

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

statements :: Parser Statement
statements = buildExpressionParser statementOperators statement

statement :: Parser Statement
statement =   skip
          <|> conditional 
          <|> while
          <|> assign
          <|> parens statements

statementOperators :: OperatorTable Char () Statement
statementOperators = [[binary ";" compound AssocLeft]]

compound :: Statement -> Statement -> Statement
compound s1 s2 = Compound s1 s2 

skip :: Parser Statement
skip = do reserved "skip"
          return (Skip)

conditional :: Parser Statement
conditional = do reserved "if"
                 condition <- booleanExpression
                 reserved "then"
                 trueStatement <- statement
                 reserved "else"
                 falseStatement <- statement
                 return (If condition trueStatement falseStatement)

while :: Parser Statement
while = do reserved "while"
           condition <- booleanExpression
           reserved "do"
           s <- statement
           return (While condition s)

assign :: Parser Statement
assign = do name <- identifier
            reservedOp ":="
            value <- arithmeticExpression
            return (Assign name value)

booleanExpression = buildExpressionParser booleanOperators boolean

boolean :: Parser BooleanExp
boolean =   true 
        <|> false
        <|> arithmeticComparison
        <|> parens booleanExpression

booleanOperators :: OperatorTable Char () BooleanExp
booleanOperators = [[prefix "!" Parser.not],
                    [binary "&" Parser.and AssocLeft]]

arithmeticComparison :: Parser BooleanExp
arithmeticComparison = do { l <- arithmeticExpression;
                            do { reservedOp "=";
                                 equals l}
                          <|>
                            do { reservedOp "<=";
                                 lessOrEquals l}
                           }

equals :: ArithmeticExp -> Parser BooleanExp
equals l  = do r <- arithmeticExpression
               return (Equal l r)

lessOrEquals :: ArithmeticExp -> Parser BooleanExp
lessOrEquals l = do r <- arithmeticExpression
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

arithmeticExpression = buildExpressionParser arithmeticOperators arithmetic

arithmetic :: Parser ArithmeticExp
arithmetic =   number 
           <|> variable
           <|> parens arithmeticExpression

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
