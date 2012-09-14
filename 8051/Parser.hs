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
  { T.reservedNames = ["let", "in"]
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
binding = Binding <$> var <* op "=" <*> lambdaForm <?> "binding"

lambdaForm :: Parser LambdaForm
lambdaForm = LF <$> many var <* op "@" <*> expr <?> "lambda form"

-- Expression parsers

expr :: Parser Expr
expr = letRec <|> app <?> "expression"

letRec :: Parser Expr
letRec = LetRec <$> (reserved "let" *> manySep binding)
                <*> (reserved "in"  *> expr)
                <?> "let block"

app :: Parser Expr
app = App <$> var <*> many var <?> "application"
