module STG.Types where

-- Spineless, tagless, G-machine

type Name    = String
type Tag     = Int

type Program = [Binding]

data Binding = Binding
  { lhs :: Name
  , rhs :: LambdaForm
  }
  deriving Show

data LambdaForm = LF
  { free :: [Name]
  , upd  :: Update
  , args :: [Name]
  , body :: Expr
  }
  deriving Show

data Expr
  = App Name [Name]
  | Constr Tag [Name]
  | LetRec [Binding] Expr
  | Case Expr [Match] Default
  deriving Show

data Match = Match
  { matchTag  :: Tag
  , matchVars :: [Name]
  , matchBody :: Expr
  }
  deriving Show

data Default = Named Name Expr | Default Expr
  deriving Show

data Update = U | N
  deriving Show
