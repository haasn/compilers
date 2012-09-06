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
  { T.reservedNames = ["let", "in", "case", "of", "default"]
  , T.identLetter   = alphaNum }

var = identifier <?> "var"
op  = reservedOp

tag :: Parser Tag
tag = fromInteger <$> (symbol "<" *> natural <* symbol ">") <?> "tag"

manySep :: Parser a -> Parser [a]
manySep p = p `sepBy` symbol ";"

-- STG language parsers

program :: Parser Program
program = Program <$> (whiteSpace *> manySep binding <* eof) <?> "program"

binding :: Parser Binding
binding = Binding <$> var <* op "=" <*> lambdaForm <?> "binding"

lambdaForm :: Parser LambdaForm
lambdaForm = updateLF <|> thunkLF <?> "lambda form"
  where
    updateLF = LF False <$> (op "\\" *> many var) <* op "->" <*> expr
    thunkLF  = LF True [] <$ op "@" <*> expr

-- Expression parsers

expr :: Parser Expr
expr = letRec <|> caseOf <|> constr <|> app <?> "expression"

letRec :: Parser Expr
letRec = LetRec <$> (reserved "let" *> manySep binding)
                <*> (reserved "in"  *> expr)
                <?> "let block"

caseOf :: Parser Expr
caseOf = Case <$> (reserved "case" *> expr)
              <*> (reserved "of"   *> many match)
              <*> defaultMatch
              <?> "case block"

constr :: Parser Expr
constr = Constr <$> tag <*> many var <?> "constructor"

app :: Parser Expr
app = App <$> var <*> many var <?> "application"

-- Match parsers

match :: Parser Match
match = Match <$> tag <*> many var <* op "->" <*> expr <?> "pattern match"

defaultMatch :: Parser Expr
defaultMatch = reserved "default" *> op "->" *> expr <?> "default match"
