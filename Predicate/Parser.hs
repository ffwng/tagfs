module Predicate.Parser (
	Expr(..), BOp(..), Op(..), Var, Val(..),
	expression, fullExpression, parseString
) where

import Control.Monad.Identity
import Control.Applicative ((<$>), (<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

data Expr = Const Bool
			| Leaf Var
			| LeafOp Op Var Val
			| Not Expr
			| Branch BOp Expr Expr
			deriving (Show)

data BOp = And | Or deriving (Show)

data Op = Equals | Greater | Less deriving (Show)

type Var = String

data Val = StringVal String | IntVal Integer
			deriving (Show)

languageDef :: LanguageDef st
languageDef = emptyDef
	{ Token.commentStart = "/*"
	, Token.commentEnd = "*/"
	, Token.commentLine = "//"
	, Token.identStart = letter
	, Token.identLetter = alphaNum
	, Token.reservedNames = ["true", "false"]
	, Token.reservedOpNames = [ ":", "=", "<", ">", "&&", "||", "!"]
	}

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer <|> Token.stringLiteral lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

fullExpression :: Parser Expr
fullExpression = expression <* eof

expression :: Parser Expr
expression = buildExpressionParser operators term

operators :: OperatorTable String () Identity Expr
operators = [ [Prefix (reservedOp "!" >> return Not)]
			, [Infix (reservedOp "&&" >> return (Branch And)) AssocLeft]
			, [Infix (reservedOp "||"  >> return (Branch Or)) AssocLeft]
			]

term :: Parser Expr
term = parens expression
	<|> (reserved "true" >> return (Const True))
	<|> (reserved "false" >> return (Const False))
	<|> leaf

value :: Parser Val
value = StringVal <$> identifier
	  <|> IntVal <$> integer

leaf :: Parser Expr
leaf = do
	a1 <- identifier
	rel a1 <|> return (Leaf a1)
	where rel a1 = do
		op <- relation
		a2 <- value
		return $ LeafOp op a1 a2

relation :: Parser Op
relation = (reservedOp ">" >> return Greater)
	<|> (reservedOp "<" >> return Less)
	<|> (reservedOp ":" >> return Equals)
	<|> (reservedOp "=" >> return Equals)

parseString :: String -> Maybe Expr
parseString str = case parse fullExpression "" str of
	Left _ -> Nothing
	Right r -> Just r
