module Core.Parser where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec         hiding (space)
import Text.Parsec.Expr
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import qualified Text.Parsec.Token as P

import Core.Common

{-

The syntax for this Core dialect is showcased with a small example:

 twice f x = f (f x) ;

 false = {0,0} ;
 true  = {1,0} ;

 nil       = {0,0} ;
 cons x xs = {1,2} x xs ;

 fix f = let x = f x in x ;

 null l = case l of
   <0>      -> true
   <1> x xs -> false

-}

parse :: String -> Either ParseError CoreProgram
parse = runParser coreProgram () ""

-- Lexing rules and convenience renames

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ haskellStyle
  { P.reservedNames   = ["let","in","case","of"]
  , P.reservedOpNames = ["=","->"]
  }

space    = P.whiteSpace lexer
name     = P.identifier lexer <?> "name"
op       = P.reservedOp lexer
symbol   = P.symbol lexer
int      = fromInteger <$> P.natural lexer <?> "int"
parens   = P.parens lexer
reserved = P.reserved lexer
call n x = x <?> n

-- Core language parsers

coreProgram :: Parser CoreProgram
coreProgram = CoreProgram <$>
  (space *> coreScDef `sepBy` symbol ";" <* eof) <?> "program"

coreScDef :: Parser ScDef
coreScDef = ScDef <$> name <*> many name <* op "=" <*> expr <?> "definition"

expr :: Parser CoreExpr
expr = buildExpressionParser table primexpr<|> letrec <|> caseof
       <?> "expression"
  where
    table = [[Infix (return App) AssocLeft]]

primexpr :: Parser CoreExpr
primexpr = parens expr <|> constr <|> free <?> "primitive expression"

free :: Parser CoreExpr
free = Free <$> name <?> "free variable"

constr :: Parser CoreExpr
constr = call "constructor" $ do
  symbol "{"
  t <- int <?> "tag"
  symbol ","
  a <- int <?> "arity"
  symbol "}"
  return (Constr t a)

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
