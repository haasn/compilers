{-# LANGUAGE RecordWildCards #-}
module STG.Parser where

-- Parser for STG programs

import Control.Applicative hiding (many, (<|>))
import Text.Parsec         hiding (space)
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import qualified Text.Parsec.Token as T

import STG.Types

parse :: String -> Either ParseError Program
parse = runParser program () ""

-- Lexing rules and other helpers

T.TokenParser{..} = T.makeTokenParser $ haskellStyle
  { T.reservedNames = ["let", "in", "case", "of"]
  , T.identLetter   = alphaNum }

var = identifier <?> "var"
op  = reservedOp

tag :: Parser Tag
tag = fromInteger <$> (op "<" *> natural <* op ">") <?> "tag"

manySep :: Parser a -> Parser [a]
manySep p = p `sepBy` symbol ";"

-- STG language parsers

program :: Parser Program
program = Program <$> (whiteSpace *> manySep binding <* eof) <?> "program"

binding :: Parser Binding
binding = (,) <$> var <* op "=" <*> expr <?> "binding"

-- Expression parsers

expr :: Parser Expr
expr = lambda <|> letRec <|> caseOf <|> constr <|> primop <|> app <|> atom
       <?> "expression"

lambda :: Parser Expr
lambda = Lambda <$ op "\\" <*> many var <* op "->" <*> expr <?> "lambda"

letRec :: Parser Expr
letRec = LetRec <$> (reserved "let" *> manySep binding)
                <*> (reserved "in"  *> expr)
                <?> "let block"

caseOf :: Parser Expr
caseOf = Case <$> (reserved "case" *> expr)
              <*> (reserved "of"   *> op "{" *> many match <* op "}")
              <?> "case block"

constr :: Parser Expr
constr = Constr <$> tag <*> many atom <?> "constructor"

lit :: Parser Expr
lit = Literal <$ op "#" <*> natural <?> "integer literal"

primop :: Parser Expr
primop = Prim <$> o <*> atom <*> atom <?> "primitive operation"
  where o = Add <$ op "+" <|> Mul <$ op "*" <|> Sub <$ op "-" <|> Div <$ op "/"

app :: Parser Expr
app = App <$> var <*> many atom <?> "application"

atom :: Parser Expr
atom = free <|> lit <|> parens expr <?> "atomic expression"

free :: Parser Expr
free = App <$> var <*> pure [] <?> "free variable"

-- Match parsers

match :: Parser Match
match = Match <$> tag <*> many var <* op "->" <*> expr <?> "pattern match"

defaultMatch :: Parser Expr
defaultMatch = reserved "default" *> op "->" *> expr <?> "default match"
