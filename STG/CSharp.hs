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
  put ("_" ++ n ++ ".f = delegate {")

  indent $ do
    forM_ args $ \n -> put ("var _" ++ n ++ " = args.Pop ();")
    unless (null args) br

    putExpr body

  put "};"
  br

putExpr :: Expr -> Gen ()
putExpr (App n as) = do
  forM_ (reverse as) $ \a -> put ("args.Push (_" ++ a ++ ");")
  put ("return _" ++ n ++ ";")

putExpr (LetRec bs e) = putBindings bs >> putExpr e

putExpr (Constr t ns) = do
  put ("tag = " ++ show t ++ ";")
  put "vars.Clear ();"
  forM_ ns $ \n -> put ("vars.Push (_" ++ n ++ ");")
  put "return cont.Pop ();"

putExpr (Case e ms d) = do
  put "cont.Push (new Fun (delegate {"
  indent $ do
    put "switch (tag) {"
    indent $ do
      mapM_ putMatch ms

      put "default:"
      indent (putExpr d)
    put "}"
  put "}));"
  br
  putExpr e

putMatch :: Match -> Gen ()
putMatch Match{..} = do
  put ("case " ++ show matchTag ++ ":")
  indent $ do
    forM_ (reverse matchVars) $ \n -> put ("var _" ++ n ++ " = vars.Pop ();")
    putExpr matchBody
  br

preamble :: Gen ()
preamble = do
  put "using System;"
  put "using System.Collections.Generic;"
  br
  put "class Fun {"
  put "  public FunPtr f;"
  put "  public Fun () {}"
  put "  public Fun (FunPtr p) { f = p; }}"
  put "delegate Fun FunPtr ();"
  br
  put "class STG { static void Main() {"
  br
  put "int tag = 0;"
  br
  put "var args = new Stack<Fun> ();"
  put "var cont = new Stack<Fun> ();"
  put "var vars = new Stack<Fun> ();"
  br

epilogue :: Gen ()
epilogue = do
  put "var output = new Fun ();"
  put "output.f = delegate {"
  put "  Console.WriteLine (tag);"
  put "  Environment.Exit (0);"
  put "  return null;"
  put "};"
  br
  put "cont.Push (output);"
  br
  put "var next = _main;"
  put "while (true)"
  put "  next = next.f ();"
  br
  put "}}"

-- Helpers and minor functions

alloc :: Name -> Gen ()
alloc n = put ("var _" ++ n ++ " = new Fun ();")

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
