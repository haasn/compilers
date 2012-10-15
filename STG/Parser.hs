{-# LANGUAGE RecordWildCards #-}
module STG.Parser where

-- Parser for STG programs

import Control.Applicative hiding (many, (<|>))
import Data.Char (isSpace)
import Data.Monoid (Monoid, mconcat)

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
  , T.identLetter   = alphaNum <|> char '_' }

var = identifier <?> "variable"
op  = reservedOp

enclose :: Char -> Char -> Parser String
enclose s e = symbol [s] *> many (noneOf [e]) <* symbol [e]

manySep :: Parser a -> Parser [a]
manySep p = p `sepEndBy` symbol ";"

collect :: Monoid a => [Parser a] -> Parser a
collect = fmap mconcat . sequence

-- STG language parsers

program :: Parser Program
program = Program <$> (whiteSpace *> manySep defn <* eof) <?> "program"

defn :: Parser Definition
defn = ffi <|> Binding <$> binding <?> "definition"

ffi :: Parser Definition
ffi = FFI <$ reserved "extern" <*> mode <*> arity <*> var <*> csharp
 where
  mode  = Func <$ reserved "func" <|> Action <$ reserved "action"
  arity = op "[" *> natural <* op "]"

csharp :: Parser CSharp
csharp = CSharp <$ op "{" <*> collect [blob, option [] block, blob] <* op "}"
 where
  blob  = many (noneOf "{}")
  block = collect [string "{", code <$> csharp, string "}"]

binding :: Parser (Name, Expr)
binding = (,) <$> var <* op "=" <*> expr <?> "binding"

-- Expression parsers

expr :: Parser Expr
expr = lambda <|> letRec <|> app <|> atom <?> "expression"

lambda :: Parser Expr
lambda = Lambda <$ op "\\" <*> many var <* op "->" <*> expr <?> "lambda"

letRec :: Parser Expr
letRec = LetRec <$> (reserved "let" *> manySep binding)
                <*> (reserved "in"  *> expr)            <?> "let block"

lit :: Parser Expr
lit = Literal <$> enclose '#' '#' <?> "literal"

app :: Parser Expr
app = App <$> var <*> many atom <?> "application"

atom :: Parser Expr
atom = lit <|> free <|> parens expr <?> "atomic expression"

free :: Parser Expr
free = App <$> var <*> pure []
