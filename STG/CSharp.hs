{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module STG.CSharp where

-- C# code generator

import STG.Types

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.DList as DL
import qualified Data.Map   as Map

type Gen = ReaderT Env (Writer (DL.DList Char))

runGen :: Gen () -> String
runGen = DL.toList . execWriter . (`runReaderT` env)
  where env = Env 0 "" Map.empty

data Env = Env
  { _level  :: Int
  , _suffix :: Name
  , _locals :: Map Name Name
  }

makeLenses ''Env

-- Code generators

putProgram :: Program -> Gen ()
putProgram Program{..} = do
  preamble
  putBindings bindings Nothing
  epilogue

putBindings :: [Binding] -> Maybe (Gen ()) -> Gen ()
putBindings bs e = do
  s <- query suffix
  localize s (map lhs bs) $ \names -> do
    let bs' = zipWith Binding names (map rhs bs)
    mapM_ (alloc . lhs) bs' >> br
    mapM_ putBinding bs'
    fromMaybe (return ()) e

putBinding :: Binding -> Gen ()
putBinding (Binding n LF{..}) = do
  name <- getName n
  put $ name ++ ".f = delegate {"

  indent . localize name args $ \args -> do
    forM_ args $ \n -> put ("var " ++ n ++ " = args.Pop ();")
    unless (null args) br

    putExpr body

  put "};"
  br

putExpr :: Expr -> Gen ()
putExpr (App n as) = do
  name <- getName n
  args <- mapM getName as
  forM_ (reverse args) $ \a -> put ("args.Push (" ++ a ++ ");")
  put $ "next = " ++ name ++ ";"

putExpr (LetRec bs e) = putBindings bs (Just $ putExpr e)

putExpr (Constr _ _) = error "Cannot compile Constr yet"
putExpr (Case _ _ _) = error "Cannot compile Case yet"

preamble :: Gen ()
preamble = do
  put "using System;"
  put "using System.Collections.Generic;"
  br
  put "class Fun { public FunPtr f; }"
  put "delegate void FunPtr ();"
  br
  put "class STG { static void Main() {"
  br
  put "Stack<Fun> args = new Stack<Fun> ();"
  put "Stack<Fun> cont = new Stack<Fun> ();"
  put "Stack<int> vals = new Stack<int> ();"
  br
  put "Fun next;"
  br

  -- For testing purposes only
  put "Fun five = new Fun ();"
  put "five.f = delegate { vals.Push (5); next = cont.Pop (); };"
  br

epilogue :: Gen ()
epilogue = do
  put "Fun output = new Fun ();"
  put "output.f = delegate {"
  put "  foreach (int i in vals)"
  put "    Console.WriteLine (i);"
  br
  put "  Environment.Exit (0);"
  put "};"
  br
  put "cont.Push (output);"
  br
  put "next = _main;"
  put "while (true)"
  put "  next.f ();"
  br
  put "}}"

-- Helpers and minor functions

getName :: Name -> Gen Name
getName n = do
  env <- query locals
  return (fromMaybe (error $ "no such name: " ++ n) (env^.at n))

alloc :: Name -> Gen ()
alloc n = do
  name <- getName n
  put $ "Fun " ++ name ++ " = new Fun ();"

indent :: Gen a -> Gen a
indent = local (level +~ 2)

localize :: Name -> [Name] -> ([Name] -> Gen a) -> Gen a
localize s ls c = local ((locals %~ Map.union m) . (suffix .~ s)) $ c ls'
  where
    ls' = map (\n -> ('_':n)++s) ls
    m  = Map.fromList (zip ls ls')

put :: String -> Gen ()
put s = do
  i <- query level
  tell $ DL.fromList (replicate i ' ')
  tell $ DL.fromList s
  br

br :: Gen ()
br = tell $ DL.singleton '\n'
