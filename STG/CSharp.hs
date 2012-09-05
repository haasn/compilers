{-# LANGUAGE RecordWildCards #-}
module STG.CSharp where

-- C# code generator

import STG.Types

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.DList as DL
import qualified Data.Map   as Map

type Gen = ReaderT Int (Writer (DL.DList Char))

runGen :: Gen () -> String
runGen = DL.toList . execWriter . (`runReaderT` 0)

-- Code generators

putProgram :: Program -> Gen ()
putProgram Program{..} = do
  preamble
  putBindings bindings
  epilogue

putBindings :: [Binding] -> Gen ()
putBindings bs = do
  mapM_ (alloc . lhs) bs
  br
  mapM_ putBinding bs

putBinding :: Binding -> Gen ()
putBinding (Binding n LF{..}) = do
  put $ "_" ++ n ++ ".f = delegate {"

  indent $ do
    forM_ args $ \n -> put ("var _" ++ n ++ " = args.Pop ();")
    unless (null args) br

    putExpr body

  put "};"
  br

putExpr :: Expr -> Gen ()
putExpr (App n as) = do
  forM_ (reverse as) $ \a -> put ("args.Push (_" ++ a ++ ");")
  put $ "next = _" ++ n ++ ";"

putExpr (LetRec bs e) = putBindings bs >> putExpr e

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
  put "Fun _five = new Fun ();"
  put "_five.f = delegate { vals.Push (5); next = cont.Pop (); };"
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

alloc :: Name -> Gen ()
alloc n = do
  put $ "Fun _" ++ n ++ " = new Fun ();"

indent :: Gen a -> Gen a
indent = local (+2)

put :: String -> Gen ()
put s = do
  i <- ask
  tell $ DL.fromList (replicate i ' ')
  tell $ DL.fromList s
  br

br :: Gen ()
br = tell $ DL.singleton '\n'
