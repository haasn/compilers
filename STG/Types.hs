module STG.Types where

-- Spineless, tagless, G-machine

type Name = String
type Tag  = Int
type Decl = (Name, Expr)

newtype Program = Program { defs :: [Definition] }

data Definition
  = Binding Decl
  | FFI Mode Name CSharp

data Mode = Func | Action

newtype CSharp = CSharp { code :: String }

data Expr
  = App Name [Expr]
  | Lambda [Name] Expr
  | LetRec [Decl] Expr
  | Literal String
