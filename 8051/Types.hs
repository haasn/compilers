module STG.Types where

type Name = String
type Tag  = Int

newtype Program = Program { bindings :: [Binding] }
  deriving Show

data Binding = Binding
  { lhs :: Name
  , rhs :: LambdaForm
  } deriving Show

data LambdaForm = LF
  { args :: [Name]
  , body :: Expr
  } deriving Show

data Expr
  = App Name [Name]
  | LetRec [Binding] Expr
  deriving Show
