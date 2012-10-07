{-# LANGUAGE RecordWildCards #-}
module STG.Parser where

-- Parser for STG programs

import Control.Applicative hiding (many, (<|>))
import Data.Char (isSpace)

import Text.Encoding.Z
import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import qualified Text.Parsec.Token as T

import STG.Types

parse :: String -> Either ParseError Program
parse = runParser program () ""

-- Lexing rules and other helpers

T.TokenParser{..} = T.makeTokenParser $ haskellStyle
  { T.reservedNames = ["let", "in", "extern", "func", "action"]
  , T.identLetter   = alphaNum }

var = quoted <|> identifier <?> "variable"
op  = reservedOp

quoted :: Parser String
quoted = zEncodeString <$ symbol "<" <*> many (noneOf ">") <* symbol ">"

manySep :: Parser a -> Parser [a]
manySep p = p `sepBy` symbol ";"

lexExcept :: Char -> Parser String
lexExcept = lexeme . many . satisfy . p
  where p t c = c /= t && not (isSpace c)

-- STG language parsers

program :: Parser Program
program = Program <$> (whiteSpace *> manySep defn <* eof) <?> "program"

defn :: Parser Definition
defn = ffi <|> Binding <$> binding <?> "definition"

ffi :: Parser Definition
ffi = FFI <$ reserved "extern" <*> mode <*> lexExcept ';' <?> "extern"
  where mode = Func   <$ reserved "func"
           <|> Action <$ reserved "action"
           <|> Field  <$ reserved "field"

binding :: Parser (Name, Expr)
binding = (,) <$> var <* op "=" <*> expr <?> "binding"

-- Expression parsers

expr :: Parser Expr
expr = lambda <|> letRec <|> app <|> atom <?> "expression"

lambda :: Parser Expr
lambda = Lambda <$ op "\\" <*> many var <* op "->" <*> expr <?> "lambda"

letRec :: Parser Expr
letRec = LetRec <$> (reserved "let" *> manySep binding)
                <*> (reserved "in"  *> expr)
                <?> "let block"

lit :: Parser Expr
lit = Literal <$ op "#" <*> lexExcept '#' <* op "#" <?> "literal"

app :: Parser Expr
app = App <$> var <*> many atom <?> "application"

atom :: Parser Expr
atom = lit <|> free <|> parens expr <?> "atomic expression"

free :: Parser Expr
free = App <$> var <*> pure []
