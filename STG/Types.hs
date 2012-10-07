module STG.Types where

-- Spineless, tagless, G-machine

type Name = String
type Tag  = Int

newtype Program = Program { defs :: [Definition] }

data Definition
  = Binding (Name, Expr)
  | FFI Mode String

data Mode = Func | Action | Field

data Expr
  = App Name [Expr]
  | Lambda [Name] Expr
  | LetRec [(Name, Expr)] Expr
  | Literal String
