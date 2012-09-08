module STG.Types where

-- Spineless, tagless, G-machine

type Name = String
type Tag  = Int

newtype Program = Program { bindings :: [Binding] }
  deriving Show

data Binding = Binding
  { lhs :: Name
  , rhs :: LambdaForm
  } deriving Show

data LambdaForm = LF
  { upd  :: Bool
  , args :: [Name]
  , body :: Expr
  } deriving Show

data Expr
  = App Atom [Atom]
  | Constr Tag [Atom]
  | LetRec [Binding] Expr
  | Case Expr [Match]
  | Prim Op Atom Atom
  deriving Show

data Atom = Name Name | Lit Integer
  deriving Show

data Op = Add | Mul | Sub | Div
  deriving Show

data Match = Match
  { matchTag  :: Tag
  , matchVars :: [Name]
  , matchBody :: Expr
  } deriving Show
