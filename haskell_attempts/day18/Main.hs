module Main where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

main :: IO ()
main = do
  f <- readFile "../../input18.txt"
  let ls = lines f
  print $ sum (map (evaluateExpr.parseString operators) ls)
  print $ sum (map (evaluateExpr.parseString operators2) ls)

data Expr = IntConst Integer
          | Binary BinOp Expr Expr
           deriving (Show)

data BinOp = Add
           | Multiply
            deriving (Show)

languageDef =
  emptyDef { Token.reservedOpNames= ["+", "*"]}

lexer = Token.makeTokenParser languageDef

parens = Token.parens lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
integer = Token.integer lexer


expression :: [[Operator Char () Expr]] -> Parser Expr
expression o = buildExpressionParser o (term o)

-- addition and multiplication have the same precedence
operators=[[addition, multiplication]]

-- addition has higher precedence than multiplication
operators2=[[addition],
            [multiplication]]

addition = Infix (reservedOp "+" >> return (Binary Add)) AssocLeft
multiplication = Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft

term o = parens (expression o)
      <|> fmap IntConst integer

parseString :: [[Operator Char () Expr]] -> String  -> Expr
parseString o str = do
  case parse (expression o) "" str of
    Left e -> error $ show e
    Right r -> r

evaluateExpr :: Expr -> Integer
evaluateExpr (IntConst x) = x
evaluateExpr (Binary Multiply x y) = (evaluateExpr x) * (evaluateExpr y)
evaluateExpr (Binary Add x y) = (evaluateExpr x) + (evaluateExpr y)
