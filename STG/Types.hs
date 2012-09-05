module STG.Types where

-- Spineless, tagless, G-machine

type Name = String
type Tag  = Int

newtype Program = Program { bindings :: [Binding] }
  deriving Show

data Binding = Binding
  { lhs :: Name
  , rhs :: LambdaForm
  }
  deriving Show

data LambdaForm = LF
  { upd  :: Update
  , args :: [Name]
  , body :: Expr
  }
  deriving Show

data Expr
  = App Name [Name]
  | Constr Tag [Name]
  | LetRec [Binding] Expr
  | Case Expr [Match] Expr
  deriving Show

data Match = Match
  { matchTag  :: Tag
  , matchVars :: [Name]
  , matchBody :: Expr
  }
  deriving Show

data Update = U | N
  deriving Show
