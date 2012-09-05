{-# LANGUAGE DeriveFunctor #-}
module STG.Types where

-- Spineless, tagless, G-machine

type Name = String
type Tag  = Int

type Program    = GProgram Name
type Binding    = GBinding Name
type LambdaForm = GLambdaForm Name
type Expr       = GExpr Name
type Match      = GMatch Name
type Default    = GDefault Name

-- Generalized versions of the above types, parametrized over variable type

newtype GProgram a = Program [GBinding a]
  deriving (Show, Functor)

data GBinding a = Binding
  { lhs :: a
  , rhs :: GLambdaForm a
  }
  deriving (Show, Functor)

data GLambdaForm a = LF
  { free :: [a]
  , upd  :: Update
  , args :: [a]
  , body :: GExpr a
  }
  deriving (Show, Functor)

data GExpr a
  = App a [a]
  | Constr Tag [a]
  | LetRec [GBinding a] (GExpr a)
  | Case (GExpr a) [GMatch a] (GDefault a)
  deriving (Show, Functor)

data GMatch a = Match
  { matchTag  :: Tag
  , matchVars :: [a]
  , matchBody :: GExpr a
  }
  deriving (Show, Functor)

data GDefault a = Named a (GExpr a) | Default (GExpr a)
  deriving (Show, Functor)

data Update = U | N
  deriving Show
