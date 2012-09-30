module STG.Types where

-- Spineless, tagless, G-machine

type Name = String
type Tag  = Int

newtype Program = Program { bindings :: [Binding] }
  deriving Show

type Binding = (Name, Expr)

data Expr
  = App Name [Expr]
  | Lambda [Name] Expr
  | LetRec [Binding] Expr
  | Constr Tag [Expr]
  | Case Expr [Match]
  | Literal Integer
  | Prim Op Expr Expr
  deriving Show

data Op = Add | Mul | Sub | Div
  deriving Show

data Match = Match
  { matchTag  :: Tag
  , matchVars :: [Name]
  , matchBody :: Expr
  } deriving Show
