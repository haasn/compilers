module Core.Parser where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec         hiding (space)
import Text.Parsec.Expr
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import qualified Text.Parsec.Token as T

import Core.Common

parse :: String -> Either ParseError CoreProgram
parse = runParser coreProgram () ""

-- Lexing rules and convenience renames

lexer :: T.TokenParser ()
lexer = T.makeTokenParser $ haskellStyle
  { T.reservedNames   = ["let","in","case","of"]
  , T.reservedOpNames = ["=","->"]
  }

space    = T.whiteSpace lexer
name     = T.identifier lexer <?> "name"
op       = T.reservedOp lexer
symbol   = T.symbol lexer
int      = fromInteger <$> T.natural lexer <?> "int"
parens   = T.parens lexer
reserved = T.reserved lexer

-- Core language parsers

coreProgram :: Parser CoreProgram
coreProgram = CoreProgram
  <$> (space *> coreScDef `sepBy` symbol ";" <* eof)
  <?> "program"

coreScDef :: Parser ScDef
coreScDef = ScDef <$> name <*> many name <* op "=" <*> expr <?> "definition"

expr :: Parser CoreExpr
expr = primexpr `chainl1` return App <|> letrec <|> caseof
       <?> "expression"

primexpr :: Parser CoreExpr
primexpr = parens expr <|> constr <|> free <?> "primitive expression"

free :: Parser CoreExpr
free = Free <$> name <?> "free variable"

constr :: Parser CoreExpr
constr = Constr
  <$> (symbol "{" *> int <?> "tag")
  <*> (symbol "," *> int <* symbol "}" <?> "arity")
  <?> "constructor"

letrec :: Parser CoreExpr
letrec = LetRec
  <$> (reserved "let" *> letdef `sepBy` symbol ";")
  <*> (reserved "in"  *> expr)
  <?> "let block"

letdef :: Parser (Name, CoreExpr)
letdef = (,) <$> name <* op "=" <*> expr <?> "let definition"

caseof :: Parser CoreExpr
caseof = Case
  <$> (reserved "case" *> expr)
  <*> (reserved "of"   *> many1 match)
  <?> "case .. of"

match :: Parser Match
match = Match
  <$> (symbol "<" *> (int <?> "tag") <* symbol ">")
  <*> many name
  <*> (op "->" *> expr)
  <?> "pattern match"
