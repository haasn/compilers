module Core.Common where

-- Shared ADT for Core source code

type Name  = String
type Tag   = Int
type Arity = Int

-- A program is just a list of definitions
newtype CoreProgram = CoreProgram [ScDef]
  deriving Show

data ScDef = ScDef
  { scName :: Name
  , scArgs :: [Name]
  , scBody :: CoreExpr
  }
  deriving Show

-- A Core expression has one of five forms:

data CoreExpr
  = Free Name             -- A free variable
  | Constr Tag Arity      -- A constructor with a given tag and arity
  | App CoreExpr CoreExpr -- An application of a function to an argument
  | LetRec Binds CoreExpr -- A recursive let block
  | Case CoreExpr [Match] -- A case/of block
  deriving Show

-- A list of name definitions
type Binds = [(Name, CoreExpr)]

-- An option in a ‘case..of’ block
data Match = Match
  { matchTag  :: Tag
  , matchVars :: [Name]
  , matchBody :: CoreExpr
  }
  deriving Show

-- A basic prelude

prelude :: [ScDef]
prelude =
  [ ScDef "I" ["x"] $ Free "x"
  , ScDef "K" ["x", "y"] $ Free "x"
  , ScDef "K1" ["x", "y"] $ Free "y"
  , ScDef "S" ["f", "g", "x"] $ App (App (Free "f") (Free "x"))
                                 (App (Free "g") (Free "x"))
  , ScDef "compose" ["f", "g", "x"] $ App (Free "f") (App (Free "g") (Free "x"))
  ]
