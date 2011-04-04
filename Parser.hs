module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

-- lexer

lexer :: P.TokenParser ()
lexer = P.makeTokenParser whileDef

whileDef = (emptyDef
              {P.reservedOpNames = ["*", "+", "-"],
               P.identStart = letter
              })

identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
integer = P.integer lexer

-- parser 

data AExp = Number Integer
          | Variable String
          | Add AExp AExp
          | Mul AExp AExp
          | Sub AExp AExp
            deriving (Show)

aexp :: Parser AExp
aexp = buildExpressionParser table arithmetic

arithmetic :: Parser AExp
arithmetic = number <|> variable

number :: Parser AExp
number = do n <- integer
            return (Number n)

variable :: Parser AExp
variable = do var <- identifier
              return (Variable var)

table :: OperatorTable Char () AExp
table = [[binary "*" mul AssocLeft],
         [binary "+" add AssocLeft, binary "-" sub AssocLeft]]

binary symbol operation assoc = Infix (do {reservedOp symbol; 
                                           return operation}) 
                                assoc
mul :: AExp -> AExp -> AExp
mul l r = Mul l r

add :: AExp -> AExp -> AExp
add l r = Add l r

sub :: AExp -> AExp -> AExp
sub l r = Sub l r
